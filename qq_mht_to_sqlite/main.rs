use regex::Regex;
use select::{document::Document, node::Data, predicate::Name};
use serde_json::json;
use rusqlite::{Connection, Transaction};
use std::{
    fs::{File, read_dir},
    io::{BufRead, BufReader, Error, ErrorKind, Read, prelude::*},
    path::{Path, PathBuf}, collections::HashMap
};

trait ExactlyOne: Iterator {
    fn exactly_one(self) -> Result<Self::Item, Error>;
}

impl<T: Iterator> ExactlyOne for T {
    fn exactly_one(self) -> Result<Self::Item, Error> {
        let mut iter = self;
        let first = iter.next();
        if first.is_none() {
            return Err(Error::new(ErrorKind::InvalidData, "no element"));
        }
        if iter.next().is_some() {
            return Err(Error::new(ErrorKind::InvalidData, "more than one element"));
        }
        Ok(first.unwrap())
    }
}

enum MhtParseStatus {
    MhtmlHead,
    HtmlHead,
    HtmlBody,
    ImageHead,
    ImageBody,
}

struct MhtHtmlPart {
    content_transfer_encoding: String,
    content_type: String,
    data: MhtHtmlBody
}

struct MhtImagePart {
    content_transfer_encoding: String,
    content_type: String,
    content_location: String,
    data: String
}

struct MhtHeadContentType {
    content_type: String,
    charset: String,
    _type: String,
    boundary: String
}

struct MhtHead {
    from: String,
    subject: String,
    mime_version: String,
    content_type: MhtHeadContentType
}

enum MhtHtmlBody {
    Raw(String),
    Parsed(Document)
}

enum GxMsg {
    Text(String),
    Image(String),
}

static DEBUG_ONLY: bool = false;

fn get_head_value(data: &str) -> String {
    data[data.find(":").unwrap() + 1..].trim().to_string()
}

fn insert_data_impl_file_impl_html_impl_parse_txt_impl_font(data: &select::node::Node) -> GxMsg {
    GxMsg::Text(data.children().map(|ch| {
        match ch.data() {
            Data::Text(text) => text.to_string(),
            Data::Element(name, _) if name.local.to_string() == "br" => "\n".to_string(),
            Data::Element(name, _) => panic!("unknow element: {}", name.local.to_string()),
            _ => panic!("unknown data"),
        }
    }).collect::<Vec<String>>().join(""))
}

fn insert_data_impl_file_impl_html_impl_parse_txt(data: &select::node::Node) -> GxMsg {
    match data.data() {
        Data::Element(name, _) if name.local.to_string() == "font" => {
            insert_data_impl_file_impl_html_impl_parse_txt_impl_font(data)
        },
        Data::Element(name, _) if (|tag| tag == "b" || tag == "u" || tag == "i")(name.local.to_string()) => {
            insert_data_impl_file_impl_html_impl_parse_txt(&data.children().exactly_one().unwrap())
        },
        Data::Element(name, _) => {
            panic!("unknow tag: {}", name.local.to_string())
        },
        _ => panic!("unknown data")
    }
}

fn insert_data_impl_file_impl_html(sqlite_db: &Transaction, html_part: &mut MhtHtmlPart, mapping: &HashMap<String, String>, dev: &str) {
        html_part.data = match &html_part.data {
            MhtHtmlBody::Raw(buf) => MhtHtmlBody::Parsed(Document::from(buf.as_str())),
            MhtHtmlBody::Parsed(_) => panic!("wtf"),
        };
        let html = match &html_part.data {
            MhtHtmlBody::Raw(_) => panic!("pls parse"),
            MhtHtmlBody::Parsed(d) => d,
        };

        let mut current_title = String::new();

        let mut current_date = String::new();
        let mut current_group = String::new();
        let mut current_session = String::new();
    
        let body = html.find(Name("body")).exactly_one().unwrap();
        let table = body.children().filter(|n| match n.data() {
            Data::Element(nt, _) if nt.local.to_string() == "table" => true,
            _ => false
        }).exactly_one().unwrap();
        table.children().exactly_one().unwrap().children()
            .for_each(|tr| {
                let td = tr.children().filter(|n| match n.data() {
                    Data::Element(nt, _) if nt.local.to_string() == "td" => true,
                    _ => false
                }).exactly_one().unwrap();
                let td_tab = td.find(Name("table")).next();
                if td_tab.is_some() {
                    let obj_div = td.children().nth(0).unwrap();
                    assert_eq!(obj_div.attr("style").unwrap(), "color:#000000;padding-left:10px;font-weight:bold;");

                    let cur_obj = obj_div.children().exactly_one().unwrap().as_text().unwrap();

                    let msg = td_tab.unwrap().find(Name("td")).exactly_one().unwrap().children().map(|dv| {
                        let fnt = dv.children().exactly_one().unwrap();
                        match fnt.data() {
                            Data::Element(tg, _) if tg.local.to_string() == "font" => (),
                            _ => panic!("$hit")
                        };

                        // fnt.children().exactly_one().unwrap().as_text().unwrap()
                        fnt.children().map(|fd| match fd.data() {
                            Data::Text(_) => fd.as_text().unwrap(),
                            Data::Element(tg, _) if tg.local.to_string() == "br" => "\n",
                            _ => panic!("$hit")
                        }).collect::<Vec<&str>>().join("")
                    }).collect::<Vec<String>>().join("\n");

                    let ins_device = dev;
                        let ins_group = &current_group;
                        let ins_session = &current_session;

                        let ins_time = format!("{} {}",
                            &current_date,
                            "00:00:00");

                        let ins_object = cur_obj;

                    let ins_message = json!([{ "txt": msg }]).to_string();

                    if DEBUG_ONLY {
                        println!("{} | {} | {} | {} | {} | {}",
                            ins_device, 
                            ins_group, 
                            ins_session, 
                            ins_time, 
                            ins_object, 
                            &ins_message);
                        return;
                    }
                
                    sqlite_db
                        .execute(
                            r#"insert into records (device, "group", session, time, object, message) values (:device, :group, :session, :time, :object, :message)"#,
                            rusqlite::named_params! {
                                ":device": ins_device,
                                ":group": ins_group,
                                ":session": ins_session,
                                ":time": ins_time,
                                ":object": ins_object,
                                ":message": &ins_message
                            }
                        )
                        .unwrap();

                    return;
                }
                match td.children().count() {
                    1 => {
                        if td.attrs().count() > 0 {
                            assert_eq!(td.attr("style").unwrap(), "border-bottom-width:1px;border-bottom-color:#8EC3EB;border-bottom-style:solid;color:#3568BB;font-weight:bold;height:24px;line-height:24px;padding-left:10px;margin-bottom:5px;");
                            let text = td.children().exactly_one().unwrap().as_text().unwrap();
                            assert!(text.starts_with("日期:"));
                            current_date = text[text.find(':').unwrap() + 1..].trim().to_string();
                        } else {
                            let div = td.find(Name("div")).exactly_one().unwrap();
                            assert_eq!(div.attr("style").unwrap(), "padding-left:10px;");
    
                            if div.children().count() > 1 {
                                let b = div.find(Name("b")).exactly_one().unwrap().children().exactly_one().unwrap();
                                current_title = b.as_text().unwrap().to_string();
                            } else {
                                let text = div.children().exactly_one().unwrap().as_text().unwrap();
                                if text.starts_with("消息分组:") {
                                    // current_date = String::new();
                                    // current_session = String::new();
                                    current_group = text[text.find(':').unwrap() + 1..].to_string();
                                } else if text.starts_with("消息对象:") {
                                    current_session = text[text.find(':').unwrap() + 1..].to_string();
                                } else if text.trim().is_empty() {
    
                                } else {
                                    panic!("unknown text: {text}");
                                }
                            }
                        }
                    },
                    2 => {
                        let mut child = td.children();
                        let from = child.next().unwrap();
                        let msg = child.next().unwrap();

                        let ins_device = dev;
                        let ins_group = &current_group;
                        let ins_session = &current_session;

                        let ins_time = format!("{} {}",
                            &current_date,
                            from.children().nth(1).unwrap().as_text().unwrap().to_string());

                        let ins_object = |raw_ch:select::node::Node| -> Option<String> {
                            if raw_ch.children().count() == 0 {
                                return None
                            }
                            Some(raw_ch.children().exactly_one().unwrap().as_text().unwrap().to_string())
                        }(from.children().nth(0).unwrap());

                        let message = msg.children().map(|data| {
                            match data.data() {
                                Data::Element(name, _) => {
                                    let tag = name.local.to_string();
                                    if tag == "img" {
                                        let img_name = data.attr("src").unwrap();
                                        assert!(Regex::new(r#"^\{[\dA-Fa-f]{8}-(?:[\dA-Fa-f]{4}-){3}[\dA-Fa-f]{12}\}\.dat$"#).unwrap().is_match(img_name));
                                        GxMsg::Image(img_name.to_string())
                                    } else if tag == "font" {
                                        insert_data_impl_file_impl_html_impl_parse_txt_impl_font(&data)
                                    } else if tag == "b" || tag == "u" || tag == "i" {
                                        insert_data_impl_file_impl_html_impl_parse_txt(&data.children().exactly_one().unwrap())
                                    } else {
                                        panic!("unknown tag: {tag}");
                                    }
                                },
                                _ => panic!("unknown msg")
                            }
                        }).collect::<Vec<GxMsg>>();

                        let json_msg = serde_json::Value::Array(message.iter().map(|x| {
                            match x {
                                GxMsg::Text(text) => json!({ "txt": text }),
                                GxMsg::Image(url) => json!({ "img": mapping.get(url).unwrap_or(url) }),
                            }
                        }).collect::<Vec<serde_json::Value>>()).to_string();

                        if DEBUG_ONLY {
                            println!("{} | {} | {} | {} | {} | {}",
                                ins_device, 
                                ins_group, 
                                ins_session, 
                                ins_time, 
                                ins_object.unwrap_or("NULL".to_string()), 
                                json_msg);
                            return;
                        }

                        sqlite_db
                            .execute(
                                r#"insert into records (device, "group", session, time, object, message) values (:device, :group, :session, :time, :object, :message)"#,
                                rusqlite::named_params! {
                                    ":device": ins_device,
                                    ":group": ins_group,
                                    ":session": ins_session,
                                    ":time": &ins_time,
                                    ":object": ins_object,
                                    ":message": json_msg
                                }
                            )
                            .unwrap();
                    },
                    _ => panic!("unknown td count")
                }
            });
}

fn insert_data_impl_file_impl_image(image_part: &MhtImagePart, img_path: &PathBuf, mapping: &mut HashMap<String, String>) {
    let data = base64::decode(&image_part.data.as_str()).unwrap();
    let md5 = format!("{:x}", md5::compute(&data));

    let cat = img_path.join(&md5[..2]);
    if !Path::exists(&cat) {
        std::fs::create_dir(&cat).unwrap();
    }

    let file_name = format!("{}.{}", md5, &image_part.content_type.split('/').skip(1).exactly_one().unwrap());
    let file_path = cat.join(&file_name);
    if !Path::exists(&file_path) {
        let mut fs = File::create(&file_path).unwrap();
        fs.write_all(data.as_slice()).unwrap();
    }

    mapping.insert(image_part.content_location.clone(), file_name);
}

fn insert_data_impl_file(sqlite_db: &Transaction, path: &PathBuf, img_path: &PathBuf, dev: &str) {
    let file = File::open(&path).unwrap();
    let mut reader = BufReader::new(file);

    let mut img_mapping = HashMap::<String, String>::new();

    let mut status = MhtParseStatus::MhtmlHead;
    let mut mhtml_head = MhtHead {
        from: String::new(),
        subject: String::new(),
        mime_version: String::new(),
        content_type: MhtHeadContentType {
            content_type: String::new(),
            charset: String::new(),
            _type: String::new(),
            boundary: String::new(),
        }
    };

    let mut x = [0; 3];
    reader.read_exact(&mut x).unwrap();
    assert_eq!(x, [0xEF, 0xBB, 0xBF]);

    let mut html_buf = MhtHtmlPart {
        content_transfer_encoding: String::new(),
        content_type: String::new(),
        data: MhtHtmlBody::Raw(String::new()),
    };

    let mut image_buf = MhtImagePart {
        content_transfer_encoding: String::new(),
        content_type: String::new(),
        content_location: String::new(),
        data: String::new()
    };

    for line_raw in reader.lines() {
        let line = line_raw.unwrap();
        match status {
            MhtParseStatus::MhtmlHead => {
                if line.starts_with("From:") {
                    mhtml_head.from = get_head_value(&line);
                } else if line.starts_with("Subject:") {
                    mhtml_head.subject = get_head_value(&line);
                } else if line.starts_with("MIME-Version:") {
                    mhtml_head.mime_version = get_head_value(&line);
                } else if line.starts_with("Content-Type:") {
                    mhtml_head.content_type.content_type =
                        get_head_value(&line).trim_end_matches(';').to_string();
                } else if line.starts_with("\tcharset") {
                    let raw = line[line.find("=").unwrap() + 1..].trim().to_string();
                    mhtml_head.content_type.charset = raw
                        .trim_start_matches('"')
                        .trim_end_matches('"')
                        .to_string();
                } else if line.starts_with("\ttype") {
                    let raw = line[line.find("=").unwrap() + 1..].trim().to_string();
                    mhtml_head.content_type._type = raw
                        .trim_end_matches(';')
                        .trim_start_matches('"')
                        .trim_end_matches('"')
                        .to_string();
                } else if line.starts_with("\tboundary") {
                    let raw = line[line.find("=").unwrap() + 1..].trim().to_string();
                    let tl = raw[raw.find('"').unwrap() + 1..].to_string();
                    mhtml_head.content_type.boundary = tl[..tl.rfind('"').unwrap()].to_string();
                } else if line.is_empty() {
                } else if line.starts_with("--") && line[2..].eq(&mhtml_head.content_type.boundary) {
                    assert_eq!(mhtml_head.from, "<Save by Tencent MsgMgr>");
                    assert_eq!(mhtml_head.subject, "Tencent IM Message");
                    assert_eq!(mhtml_head.mime_version, "1.0");
                    assert_eq!(mhtml_head.content_type.content_type, "multipart/related");
                    assert_eq!(mhtml_head.content_type.charset, "utf-8");
                    assert_eq!(mhtml_head.content_type._type, "text/html");
                    status = MhtParseStatus::HtmlHead;
                } else {
                    panic!("unknown line: {line}");
                }
            }
            MhtParseStatus::HtmlHead => {
                if line.starts_with("Content-Type:") {
                    html_buf.content_type = get_head_value(&line);
                } else if line.starts_with("Content-Transfer-Encoding:") {
                    html_buf.content_transfer_encoding = get_head_value(&line);
                } else if line.is_empty() {
                    assert_eq!(html_buf.content_type, "text/html");
                    assert_eq!(html_buf.content_transfer_encoding, "7bit");
                    status = MhtParseStatus::HtmlBody;
                } else {
                    panic!("[HtmlHead] unknown line: {}", line);
                }
            }
            MhtParseStatus::HtmlBody => {
                if line.starts_with("--") && line[2..].eq(&mhtml_head.content_type.boundary) {
                    if DEBUG_ONLY {
                        insert_data_impl_file_impl_html(sqlite_db, &mut html_buf, &img_mapping, &dev);
                        return;
                    }
                    status = MhtParseStatus::ImageHead;
                } else {
                    match &mut html_buf.data {
                        MhtHtmlBody::Raw(buf) => buf.push_str(&line),
                        MhtHtmlBody::Parsed(_) => panic!("wtf")
                    }
                }
            }
            MhtParseStatus::ImageHead => {
                if line.starts_with("Content-Type:") {
                    image_buf.content_type = get_head_value(&line);
                } else if line.starts_with("Content-Transfer-Encoding:") {
                    image_buf.content_transfer_encoding = get_head_value(&line);
                } else if line.starts_with("Content-Location:") {
                    image_buf.content_location = get_head_value(&line);
                } else if line.is_empty() {
                    assert!(image_buf.content_type.starts_with("image/"));
                    assert_eq!(image_buf.content_transfer_encoding, "base64");
                    assert!(Regex::new(r#"^\{[\dA-Fa-f]{8}-(?:[\dA-Fa-f]{4}-){3}[\dA-Fa-f]{12}\}\.dat$"#).unwrap().is_match(&image_buf.content_location));
                    status = MhtParseStatus::ImageBody;
                } else {
                    panic!("unknown line: {line}");
                }
            }
            MhtParseStatus::ImageBody => {
                if line.starts_with("--") && line[2..].eq(&mhtml_head.content_type.boundary) {
                    insert_data_impl_file_impl_image(&image_buf, img_path, &mut img_mapping);
                    image_buf = MhtImagePart {
                        content_transfer_encoding: String::new(),
                        content_type: String::new(),
                        content_location: String::new(),
                        data: String::new(),
                    };
                    status = MhtParseStatus::ImageHead;
                } else if line.starts_with("--")
                    && line.ends_with("--")
                    && line[2..line.len() - 2].starts_with(&mhtml_head.content_type.boundary)
                {
                    insert_data_impl_file_impl_image(&image_buf, img_path, &mut img_mapping);
                    break;
                } else {
                    image_buf.data.push_str(&line);
                }
            }
        }
    }

    insert_data_impl_file_impl_html(sqlite_db, &mut html_buf, &img_mapping, &dev);
}

fn insert_data(sqlite_db: &Transaction, dev: &str, base_path: &str) {
    let img_path = Path::new(base_path).join("img");
    // let img_path = Path::new(r#"\\TreeDiagram\K\qq_mht"#).join("img");
    for file in read_dir(Path::new(base_path).join(dev)).unwrap() {
        let mht_file = file.unwrap().path();
        print!("=> {:?}...", &mht_file);
        if mht_file.ends_with(".mht") || mht_file.extension().unwrap_or_default() == "mht" {
            insert_data_impl_file(&sqlite_db, &mht_file, &img_path, &dev);
            println!("done");
        } else {
            println!("");
        }
    }
}

fn main() {
    let db_path = r#"K:\qq_mht.22.11.30\qq_mht.sqlite"#;
    // let db_path = r#"E:\tmp\qq_mht.sqlite"#;
    let shit_path = r#"K:\qq_mht.22.11.30"#;

    let mut conn = Connection::open(db_path).unwrap();
    let trans = conn.transaction().unwrap();

    // insert_data(&trans, "Trident", shit_path);
    // insert_data(&trans, "Augma", shit_path);
    // insert_data(&trans, "TreeDiagram", shit_path);
    insert_data(&trans, "CAI", shit_path);
    // let path = Path::new(r#"E:\tmp\qq_shit_22_11_12\test"#).join("验证消息.mht");
    // let img_path = Path::new(r#"\\TreeDiagram\K\qq_mht"#).join("img");
    // insert_data_impl_file(&trans, &path, &img_path, "Test");

    trans.commit().unwrap();
    conn.close().unwrap();
}
