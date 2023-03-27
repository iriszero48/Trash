use std::{
    fs::File,
    io::{self, BufRead},
};

fn main() {
    if std::env::args().len() != 3 {
        eprintln!("args.len() = {} in args.len() != 3", std::env::args().len(),);
        println!(
            "Usage: {} prefix raw_file",
            std::env::args().nth(0).unwrap()
        );
        std::process::exit(1);
    }

    let prefix = std::env::args().nth(1).unwrap();
    let raw_path = std::env::args().nth(2).unwrap();

    let key = format!("{}_原始值", prefix);

    let fs = File::open(raw_path).unwrap();
    let lines = io::BufReader::new(fs).lines();

    for line in lines {
        let line = line.unwrap();
        if (!line.is_empty()) {
            let mut dict = serde_json::Map::new();
            dict.insert(key.clone(), serde_json::Value::String(line));
            println!("{}", serde_json::to_string(&dict).unwrap());
        }
    }
}
