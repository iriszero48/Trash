use rusqlite::{Connection, Transaction};
use std::{
    fs::{read_dir, File},
    io::{BufRead, BufReader}
};

static DB_PATH: &str = r#"E:\tmp\internet_dataset\internet_dataset.sqlite"#;
static KEY_DUMP: &str = r#"E:\tmp\internet_dataset\key_dump"#;

fn insert_data(trans: &Transaction) {
    let mut count = 0;
    for path in read_dir(KEY_DUMP).unwrap().map(|p| p.unwrap().path()) {
        // let fs = BufReader::new(File::open(path).unwrap());
        // let mut buf = String::new();
        // fs.lines().for_each(|l| buf.push_str(l.unwrap().as_str()));
        let buf = BufReader::new(File::open(path).unwrap()).lines().nth(0).unwrap().unwrap();

        trans.execute(
            "insert into key values (:data)",
            rusqlite::named_params![":data": &buf]
        ).unwrap();
        count += 1;
        if count % 100000 == 0 {
            println!("{} keys inserted", count);
        }
    }
    println!("{} keys inserted", count);
}

fn main() {
    let mut conn = Connection::open(DB_PATH).unwrap();
    let trans = conn.transaction().unwrap();

    insert_data(&trans);

    trans.commit().unwrap();
    conn.close().unwrap();
}
