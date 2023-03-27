use rusqlite::Connection;
use std::fs::{read_dir, File};
use std::io::{BufRead, BufReader};

static DATABASE_PATH: &str = r#""#;
static TABLE_NAME: &str = r#""#;
static JSON_TXT_PATH: &str = r#""#;

fn main() {
    let mut conn = Connection::open(DATABASE_PATH).unwrap();
    let trans = conn.transaction().unwrap();

    // trans.execute(
    //     "create table sese (data text NOT NULL PRIMARY KEY)",
    //     rusqlite::NO_PARAMS,
    // );

    let insert_sql = format!("insert into {} values (?1)", TABLE_NAME);
    for filename in read_dir(JSON_TXT_PATH).unwrap() {
        let filename = filename.unwrap();
        println!("-> cat {}", filename.path().to_str().unwrap());

        let fs = File::open(filename.path()).unwrap();
        let reader = BufReader::new(fs);

        for line in reader.lines() {
            let line = line.unwrap();
            trans
                .execute(&insert_sql, rusqlite::params![&line.to_string()])
                .unwrap();
        }
    }

    trans.commit().unwrap();
}
