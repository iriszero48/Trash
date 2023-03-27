fn main() {
    if std::env::args().len() != 3 {
        eprintln!("args.len() = {} in args.len() != 3", std::env::args().len(),);
        println!(
            "Usage: {} prefix csv_file",
            std::env::args().nth(0).unwrap()
        );
        std::process::exit(1);
    }

    let prefix = std::env::args().nth(1).unwrap();
    let csv_path = std::env::args().nth(2).unwrap();

    let mut reader = csv::Reader::from_path(csv_path).unwrap();

    let mut head: Vec<String> = Vec::new();
    {
        let raw_head = &reader.headers().unwrap();
        let col_num = raw_head.len();
        for i in 0..col_num {
            if i == col_num - 1 && raw_head[i].is_empty() {
                continue;
            }
            head.push(format!("{}_{}", prefix, raw_head[i].to_string()))
        }
    }

    for rec in reader.records() {
        let rec = rec.unwrap();
        let mut dict = serde_json::Map::new();
        for i in 0..head.len() {
            dict.insert(
                head[i].clone(),
                match rec[i].to_string() {
                    v if v.is_empty() => serde_json::Value::Null,
                    v => serde_json::Value::String(v),
                },
            );
        }
        println!("{}", serde_json::to_string(&dict).unwrap())
    }
}
