use rayon::prelude::*;
use std::fs::File;
use std::io::{self, prelude::*, BufReader};

mod my_reader {
    use std::{
        fs::File,
        io::{self, prelude::*},
        rc::Rc,
    };

    pub struct BufReader {
        reader: io::BufReader<File>,
        buf: Rc<String>,
    }

    fn new_buf() -> Rc<String> {
        Rc::new(String::with_capacity(1024)) // Tweakable capacity
    }

    impl BufReader {
        pub fn open(path: impl AsRef<std::path::Path>) -> io::Result<Self> {
            let file = File::open(path)?;
            let reader = io::BufReader::new(file);
            let buf = new_buf();

            Ok(Self { reader, buf })
        }
    }

    impl Iterator for BufReader {
        type Item = io::Result<Rc<String>>;

        fn next(&mut self) -> Option<Self::Item> {
            let buf = match Rc::get_mut(&mut self.buf) {
                Some(buf) => {
                    buf.clear();
                    buf
                }
                None => {
                    self.buf = new_buf();
                    Rc::make_mut(&mut self.buf)
                }
            };

            self.reader
                .read_line(buf)
                .map(|u| {
                    if u == 0 {
                        None
                    } else {
                        Some(Rc::clone(&self.buf))
                    }
                })
                .transpose()
        }
    }
}

static DANBOORU_DIR: &str = r#"E:\tmp\metadata.json.tar\metadata.json\metadata\2021-old"#;
static IMAGE_DIR: &str = r#"\\treediagram\F\danbooru2021\original"#;
static DEST_DIR: &str = r#"E:\tmp\omone hokoma agm"#;

fn main() {
    let files = std::fs::read_dir(DANBOORU_DIR)
        .unwrap()
        .map(|entry| entry.unwrap().path())
        .collect::<Vec<_>>();

    files.par_iter().for_each(|path| {
        println!("Processing {:?}", path);
        let file = File::open(&path).unwrap();
        let reader = BufReader::new(file);
        for line_raw in reader.lines() {
            let data =
                serde_json::from_str::<serde_json::Value>(&line_raw.as_ref().unwrap()).unwrap();
            if data["tags"]
                .as_array()
                .unwrap()
                .to_vec()
                .iter()
                .any(|v| v["name"].as_str().unwrap() == "omone_hokoma_agm")
            {
                let id = &data["id"].as_str().unwrap();

                if str::parse::<u64>(&id).unwrap() > 4500000 {
                    let ext = &data["file_ext"].as_str().unwrap();
                    let dir = format!("0{}", &id[id.len() - 3..]);
                    let filename = format!("{}.{}", id, ext);
                    println!("{:?}", id);

                    let from = std::path::Path::new(&IMAGE_DIR).join(dir).join(&filename);
                    if std::fs::metadata(&from).is_ok() {
                        let to = std::path::Path::new(&DEST_DIR).join(&filename);
                        std::fs::copy(from, to).unwrap();
                    } else {
                        let pid = data["pixiv_id"].as_str();
                        if pid.as_ref().unwrap().is_empty() {
                            println!("not found: {}", &line_raw.unwrap())
                        } else {
                            println!("pid {}", pid.unwrap());
                        }
                    }
                }
            }
        }
    });

    // for path in std::fs::read_dir(DANBOORU_DIR).unwrap() {
    //     println!("Processing {:?}", path);
    //     let file = File::open(path.unwrap().path()).unwrap();
    //     let reader = BufReader::new(file);
    //     for line_raw in reader.lines() {
    //         let data = serde_json::from_str::<serde_json::Value>(&line_raw.unwrap()).unwrap();
    //         if data["tags"]
    //             .as_array()
    //             .unwrap()
    //             .to_vec()
    //             .iter()
    //             .any(|v| v["name"].as_str().unwrap() == "wlop")
    //         {
    //             let id = &data["id"].as_str().unwrap();
    //             let ext = &data["file_ext"].as_str().unwrap();
    //             let dir = format!("0{}", &id[id.len() - 3..]);
    //             let filename = format!("{}.{}", id, ext);
    //             println!("{:?}", data["id"]);
    //             std::fs::copy(
    //                 std::path::Path::new(&IMAGE_DIR).join(dir).join(&filename),
    //                 std::path::Path::new(&DEST_DIR).join(&filename),
    //             )
    //             .unwrap();
    //         }
    //     }
    // }
}
