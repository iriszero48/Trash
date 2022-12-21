use rusqlite::Connection;
use serde_json;
use std::{
    collections::HashSet,
    fs::{read_dir, File},
    io::{BufRead, BufReader},
    path::PathBuf,
};

static DANBOORU_DIR_PATH: &str = r#"E:\tmp\metadata.json.tar\metadata.json\metadata\2021-old"#;
static DB_PATH: &str = r#"E:\tmp\danbooru.sqlite"#;

struct ImageItem {
    id: i64,
    created_at: String,
    score: i64,
    source: Option<String>,
    md5: String,
    rating: String,
    image_width: i64,
    image_height: i64,
    file_ext: String,
    file_size: i64,
    pixiv_id: Option<String>,
}

struct TagItem {
    id: i64,
    name: String,
    category: i64,
}

fn make_str_option(data: &str) -> Option<String> {
    if data.is_empty() {
        None
    } else {
        Some(data.to_string())
    }
}

fn make_pid_option(data: &str) -> Option<String> {
    if data.is_empty() || data == "0" {
        None
    } else {
        Some(data.to_string())
    }
}

fn convert_to_int(data: &str) -> i64 {
    str::parse::<i64>(data).unwrap()
}

fn build_image_item(data: &serde_json::Value) -> ImageItem {
    ImageItem {
        id: convert_to_int(data["id"].as_str().unwrap()),
        created_at: data["created_at"].as_str().unwrap().to_string(),
        score: convert_to_int(data["score"].as_str().unwrap()),
        source: make_str_option(data["source"].as_str().unwrap()),
        md5: data["md5"].as_str().unwrap().to_string(),
        rating: data["rating"].as_str().unwrap().to_string(),
        image_width: convert_to_int(data["image_width"].as_str().unwrap()),
        image_height: convert_to_int(data["image_height"].as_str().unwrap()),
        file_ext: data["file_ext"].as_str().unwrap().to_string(),
        file_size: convert_to_int(data["file_size"].as_str().unwrap()),
        pixiv_id: make_pid_option(data["pixiv_id"].as_str().unwrap()),
    }
}

fn get_danbooru_files() -> Vec<PathBuf> {
    read_dir(DANBOORU_DIR_PATH)
        .unwrap()
        .map(|x| x.unwrap().path())
        .collect()
}

fn data_walker<F>(mut func: F)
where
    F: FnMut(serde_json::Value),
{
    for path in get_danbooru_files() {
        let fs = BufReader::new(File::open(path).unwrap());
        for line_res in fs.lines() {
            let line = line_res.unwrap();
            let data = serde_json::from_str::<serde_json::Value>(&line).unwrap();
            func(data);
        }
    }
}

fn to_images() {
    let mut conn = Connection::open(DB_PATH).unwrap();
    let trans = conn.transaction().unwrap();

    data_walker(|raw| {
        let item = build_image_item(&raw);
        trans.execute("insert into images 
        (id,created_at,score,source,md5,rating,image_width,image_height,file_ext,file_size,pixiv_id) 
        values (?1,?2,?3,?4,?5,?6,?7,?8,?9,?10,?11)",
        rusqlite::params![
            &item.id,
            &item.created_at,
            &item.score,
            &item.source,
            &item.md5,
            &item.rating,
            &item.image_width,
            &item.image_height,
            &item.file_ext,
            &item.file_size,
            &item.pixiv_id]).unwrap();
    });

    trans.commit().unwrap();
}

fn to_tags() {
    let mut conn = Connection::open(DB_PATH).unwrap();
    let trans = conn.transaction().unwrap();

    let mut dict: HashSet<i64> = HashSet::new();

    data_walker(|raw| {
        for raw_tag in raw["tags"].as_array().unwrap() {
            let tag_id = convert_to_int(raw_tag["id"].as_str().unwrap());
            if !dict.contains(&tag_id) {
                dict.insert(tag_id);

                let tag = TagItem {
                    id: tag_id,
                    name: raw_tag["name"].as_str().unwrap().to_string(),
                    category: convert_to_int(raw_tag["category"].as_str().unwrap()),
                };

                trans
                    .execute(
                        "insert into tags (id,name,category) values (:id,:name,:category)",
                        rusqlite::named_params! {
                            ":id": tag.id,
                            ":name" : tag.name,
                            ":category": tag.category
                        },
                    )
                    .unwrap();
            }
        }
    });

    trans.commit().unwrap();
}

fn to_image_tags() {
    let mut conn = Connection::open(DB_PATH).unwrap();
    let trans = conn.transaction().unwrap();

    data_walker(|raw| {
        let image_id = convert_to_int(raw["id"].as_str().unwrap());
        for raw_tag in raw["tags"].as_array().unwrap() {
            let tag_id = convert_to_int(raw_tag["id"].as_str().unwrap());

                trans
                    .execute(
                        "insert into image_tags (image_id,tag_id) values (:image_id,:tag_id)",
                        rusqlite::named_params! {
                            ":image_id": image_id,
                            ":tag_id" : tag_id
                        },
                    )
                    .unwrap();
        }
    });

    trans.commit().unwrap();
}

fn to_raw() {
    let mut conn = Connection::open(DB_PATH).unwrap();
    let trans = conn.transaction().unwrap();


    for path in get_danbooru_files() {
        let fs = BufReader::new(File::open(path).unwrap());
        for line_res in fs.lines() {
            let line = line_res.unwrap();
            trans
                    .execute(
                        "insert into raw (data) values (:data)",
                        rusqlite::named_params! {
                            ":data": line
                        },
                    )
                    .unwrap();
        }
    }

    trans.commit().unwrap();
}

fn main() {
    to_images();
    to_tags();
    to_image_tags();
    to_raw();
}
