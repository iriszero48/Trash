use std::path::{Path, PathBuf};
use std::cell::RefCell;
use std::fs::File;
use std::io::{prelude::*, BufReader};

use csv::Writer;
use snailquote::unescape;

static BASE_PATH: &'static str = r#"E:\tmp\sf"#;
static SQL_FILENAME: &'static str = r#"script.u8.sql"#;
static CSV_FILENAME: &'static str = r#"script.u8.csv"#;

thread_local!(static SQL_PATH: RefCell<PathBuf> = RefCell::new(Path::new(BASE_PATH).join(SQL_FILENAME)));
thread_local!(static CSV_PATH: RefCell<PathBuf> = RefCell::new(Path::new(BASE_PATH).join(CSV_FILENAME)));

fn main() {
    let file = File::open(SQL_PATH.with(|p| p.borrow().clone())).unwrap();
    let reader = BufReader::new(file);

    let mut csv_file = Writer::from_path(CSV_PATH.with(|p| p.borrow().clone())).unwrap();
    csv_file.write_record(&[""]).unwrap();

    let mut count = 0;

    for line_res in reader.lines() {
        let line = line_res.unwrap();
        if line.starts_with("INSERT") {
            let values_str = "VALUES".to_string();
            let pos_op = line.find(&values_str);
            if pos_op.is_none() {
                println!("can't find VALUES: {}", line);
                continue;
            }

            let pos = pos_op.unwrap();
            let values = line[pos + values_str.len()..]
                .trim()
                .trim_start_matches("(")
                .trim_end_matches(")")
                .split(",")
                .map(|v| v.trim().trim_start_matches("N"))
                .map(|v| unescape(&v).unwrap())
                .collect::<Vec<String>>();

            if values.len() != 6 {
                print!("warning: len() == {}: {:?}", values.len(), values);
            }

            csv_file.write_record(values).unwrap();
            count += 1;
        } else if line.starts_with("print") || line.starts_with("GO") {
            continue;
        } else {
            println!("ignore: {}", line);
        }
    }

    csv_file.flush().unwrap();
    println!("count: {}", count);
}
