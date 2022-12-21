use csv::ReaderBuilder;
use rusqlite::{Connection, Transaction};
use std::{
    collections::HashSet,
    fs::read_dir
};

static PCHT_DIR: &str =
    r#""#;
static DB_PATH: &str = r#"E:\tmp\pcht.sqlite"#;

fn to_id(data: &[u8]) -> i64 {
    str::parse::<i64>(std::str::from_utf8(data).unwrap()).unwrap()
}

fn to_normal_nullable_hash(data: &[u8]) -> Option<Vec<u8>> {
    if data.is_empty() {
        None
    } else {
        Some(hex::decode(std::str::from_utf8(data).unwrap()).unwrap())
    }
}

fn to_mysql_nullable_hash(data: &[u8]) -> Option<Vec<u8>> {
    if data.is_empty() {
        None
    } else {
        let str = std::str::from_utf8(data).unwrap();
        assert!(str.starts_with('*'));
        let mut res = [b'*'].to_vec();
        res.extend(hex::decode(&str[1..]).unwrap());
        Some(res)
    }
}

fn insert_data(trans: &Transaction) {
    let mut dict: HashSet<i64> = HashSet::new();
    let mut count: u64 = 0;

    for path in read_dir(PCHT_DIR).unwrap().map(|p| p.unwrap().path()) {
        let reader = ReaderBuilder::new()
            .delimiter(b'|')
            .has_headers(false)
            .flexible(true)
            .escape(Some(b'\\'))
            .double_quote(false)
            .quoting(false)
            .from_path(path)
            .unwrap();
        for row_res in reader.into_byte_records() {
            let row = row_res.unwrap();

            let id = to_id(&row[0]);
            if dict.contains(&id) {
                continue;
            }

            let pw: Vec<u8> =
                match row.len() {
                    7 => {
                        (&row[1]).to_vec()
                    },
                    x if x > 7 => {
                        let mut buf: Vec<u8> = (&row[1]).to_vec();
                        
                        for idx in 0..(x - 7) {
                            assert_eq!(buf.pop().unwrap(), b'\\');
                            buf.push(b'|');
                            buf.extend(&row[2 + idx]);
                        }

                        buf
                    },
                    x => {
                        panic!("row count: {}, {:?}", x, row);
                    }
                };

            let md5 = to_normal_nullable_hash(&row[row.len() - 5]);
            let sha1 = to_normal_nullable_hash(&row[row.len() - 4]);
            let ntlm = to_normal_nullable_hash(&row[row.len() - 3]);
            let mysql = to_mysql_nullable_hash(&row[row.len() - 2]);
            let sha256 = to_normal_nullable_hash(&row[row.len() - 1]);

            trans.execute(
                "insert into pcht (id, password, md5, sha1, ntlm, mysql, sha256)
            values (:id, :password, :md5, :sha1, :ntlm, :mysql, :sha256)",
                rusqlite::named_params![
                    ":id": &id,
                    ":password": &pw,
                    ":md5": &md5,
                    ":sha1": &sha1,
                    ":ntlm": &ntlm,
                    ":mysql": &mysql,
                    ":sha256": &sha256
                ],
            ).unwrap();

            dict.insert(id);

            count += 1;
            if count % 1000000 == 0 {
                println!("{} rows inserted", count);
            }
        }
    }

    println!("{} rows inserted", count);
}

fn main() {
    let mut conn = Connection::open(DB_PATH).unwrap();
    let trans = conn.transaction().unwrap();

    insert_data(&trans);

    trans.commit().unwrap();
    conn.close().unwrap();
}
