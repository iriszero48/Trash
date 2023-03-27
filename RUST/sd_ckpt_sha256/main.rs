use data_encoding::HEXLOWER;
use ring::digest::{Context, SHA256};
use std::collections::HashSet;
use std::ffi::OsStr;
use std::fs::{File, OpenOptions};
use std::io::{BufReader, BufWriter, Read};
use std::path::Path;
use walkdir::WalkDir;

static ROOT_PATH: &str = r#"STABLE_DIFFUSION"#;
static OUTPUT_PATH: &str = r#"sd_sha256.csv"#;

fn sha256_digest<R: Read>(mut reader: R) -> String {
    let mut context = Context::new(&SHA256);
    let mut buffer = [0; 4096];

    loop {
        let count = reader.read(&mut buffer).unwrap();
        if count == 0 {
            break;
        }
        context.update(&buffer[..count]);
    }

    HEXLOWER.encode(context.finish().as_ref())
}

fn load_csv() -> HashSet<String> {
    let mut path_set = HashSet::new();

    match csv::ReaderBuilder::new()
        .has_headers(false)
        .from_path(OUTPUT_PATH)
    {
        Err(_) => {}
        Ok(mut reader) => {
            for row in reader.records() {
                let row = row.unwrap();
                let rel_path = &row[0];
                path_set.insert(rel_path.to_string());
            }
        }
    }

    path_set
}

fn main() {
    let root_path = Path::new(ROOT_PATH);

    let path_set = load_csv();
    let csv_fs = OpenOptions::new()
        .write(true)
        .append(true)
        .create(true)
        .open(OUTPUT_PATH)
        .unwrap();
    let mut csv_writer = csv::Writer::from_writer(BufWriter::new(csv_fs));

    for file_entry in WalkDir::new(ROOT_PATH).into_iter() {
        let file_entry = file_entry.unwrap();
        let file_path = file_entry.path();
        println!("-> {:?}", &file_path);

        if !file_entry.file_type().is_file() {
            continue;
        }

        let filename = file_path
            .file_name()
            .and_then(OsStr::to_str)
            .unwrap()
            .to_ascii_lowercase();
        if ["ckpt", "safetensors", "pt"]
            .iter()
            .any(|ext| filename.ends_with(ext))
        {
            let rel_path = file_path
                .strip_prefix(root_path)
                .unwrap()
                .to_str()
                .unwrap()
                .to_string();
            if path_set.contains(&rel_path) {
                continue;
            }

            let fs = File::open(file_path).unwrap();
            let reader = BufReader::new(fs);
            let digest = sha256_digest(reader);

            csv_writer.write_record(&[rel_path, digest]).unwrap();
            csv_writer.flush().unwrap();
        }
    }
}
