const { assert } = require("node:console");
const { once } = require("node:events");
const fs = require("node:fs");
const readline = require("node:readline");
const { parseArgs, format } = require("node:util");

const options = {
    key_prefix: {
        type: "string",
        short: "p",
    },
    input_sql_path: {
        type: "string",
        short: "i",
    },
    output_path: {
        type: "string",
        short: "o",
        default: "",
    },
    encoding: {
        type: "string",
        short: "e",
    },
    auto_cast: {
        type: "boolean",
        default: false,
        short: "c",
    },
    skip_line: {
        type: "string",
        default: "0",
        short: "s",
    },
    line_map: {
        type: "string",
        default: "{}",
        short: "l",
    },
    multi_values: {
        type: "boolean",
        default: false,
        short: "m",
    },
    append: {
        type: "boolean",
        default: false,
        short: "a",
    },
    format: {
        type: "string",
        default: "%s_%s",
        short: "f",
    },
};

const { values } = parseArgs({ options });

console.log(values);

const config = {
    key_prefix: values.key_prefix,
    input_sql_path: values.input_sql_path,
    output_path: values.output_path,
    encoding: values.encoding,
    auto_cast: values.auto_cast,
    skip_line: Number(values.skip_line),
    line_map: JSON.parse(values.line_map), // Map<string, string[]>
    multi_values: values.multi_values,
    append: values.append,
    format: values.format,
};

const skip_line = [
    /^DROP TABLE IF EXISTS (\w+);$/,
    /^SET (\w*)=(.*);$/,
    /^DROP TABLE IF EXISTS [`]{0,1}(\w+)[`]{0,1};$/,
    /^LOCK TABLES `(\w*)` WRITE;$/,
    /^UNLOCK TABLES;$/,
];

const create_skip_line = [
    /^PRIMARY KEY[ ]+\((?:[`]{0,1}\w+[`]{0,1}[,]{0,1})*\)[,]{0,1}$/,
    /^UNIQUE KEY `\w+` \((?:`\w+`[,]{0,1})*\)(?: USING BTREE){0,1},{0,1}$/,
    /^KEY [`]{0,1}\w+[`]{0,1} \([`]{0,1}\w+[`]{0,1}(,[`]{0,1}\w+[`]{0,1}){0,}\)[,]{0,1}$/,
];

let noting = false;
let skip_line_eta = config.skip_line;
let creating = false;

let col_names = [];

// ...(values)...
function proc_values(values) {
    const data_arr = eval(`NULL=null,[${values}]`);
    let data = {};
    if (col_names.length) {
        assert(data_arr.length == col_names.length, `in ${values}`);
        data_arr.forEach(
            (v, i) =>
                (data[
                    format(config.format, config.key_prefix, col_names[i].k)
                ] =
                    config.auto_cast &&
                    (col_names[i].v.endsWith("int") ||
                        /^(double|float)$/i.test(col_names[i].v))
                        ? Number(v)
                        : v)
        );
    } else {
        data_arr.forEach((v, i) => (data[`${config.key_prefix}${i}`] = v));
    }
    if (config.output_path.length == 0) {
        console.log(JSON.stringify(data) + "\n");
    } else {
        fs.appendFileSync(config.output_path, JSON.stringify(data) + "\n");
    }
}

async function proc_file(file_path) {
    col_names = [];
    const fileStream = fs.createReadStream(file_path, {
        encoding: config.encoding,
    });

    const rl = readline.createInterface({
        input: fileStream,
        crlfDelay: Infinity,
    });

    rl.on("line", (line) => {
        line = line.trim();

        (line in config.line_map ? config.line_map[line] : [line]).forEach(
            (line) => {
                if (line.length == 0) return;
                if (line.startsWith("/*")) noting = true;
                if (/^CREATE TABLE [`]{0,1}(\w+)[`]{0,1} \($/.test(line)) {
                    creating = true;
                    col_names = [];
                    return;
                }
                if (noting) {
                    if (/[*]\/[;]{0,}$/.test(line)) noting = false;
                    return;
                } else if (creating) {
                    if (
                        /^\)(?:| DEFAULT| \w*=[']{0,1}[^']*[']{0,1}){0,};$/.test(
                            line
                        )
                    ) {
                        creating = false;
                        console.log(JSON.stringify(col_names));
                    } else if (create_skip_line.some((v) => v.test(line))) {
                        // ignore
                    } else if (
                        (r =
                            /^[`]{0,1}(\w*)[`]{0,1} ((?:(?:medium|tiny|small|big|)int)|char|varchar|date|datetime|decimal|enum|timestamp|double|(?:|medium)text)(?:\(\d+(,\d+){0,1}\)|\('[^']+'(?:,'[^']+'){0,}\)|)(?: (?:unsigned|NOT|NULL|auto_increment|character set utf8 collate (?:utf8_unicode_ci|utf8_general_mysql500_ci)|DEFAULT (?:'[^']*?'|NULL|CURRENT_TIMESTAMP))){0,},{0,1}$/i.exec(
                                line
                            ))
                    ) {
                        col_names.push({ k: r[1], v: r[2] });
                    } else {
                        throw Error(
                            `unknow create line: ${JSON.stringify(line)}`
                        );
                    }
                } else if (line.startsWith("/*")) {
                    noting = true;
                    return;
                } else if (line.startsWith("--")) {
                    // ignore
                } else if (
                    (r =
                        /^INSERT INTO [`]{0,1}\w+[`]{0,1} VALUES[ ]{0,1}\((.+)\);$/.exec(
                            line
                        ))
                ) {
                    if (skip_line_eta > 0) {
                        --skip_line_eta;
                        return;
                    }

                    if (config.multi_values) {
                        const regex =
                            /\(((?:(?:(?:\d*(?:\.\d*){0,1}?|'(?:[^']|\\')*?'|NULL)),{0,1})+)\),/g;
                        let new_line = `(${r[1]}),`;
                        let m;
                        while ((m = regex.exec(new_line)) !== null) {
                            if (m.index === regex.lastIndex) regex.lastIndex++;
                            proc_values(m[1]);
                        }
                    } else {
                        proc_values(r[1]);
                    }
                } else if (skip_line.some((v) => v.test(line))) {
                    // ignore
                } else {
                    throw Error(`unknow line: ${JSON.stringify(line)}`);
                }
            }
        );
    });

    await once(rl, "close");
    need_print_col_info = true;
}

function* get_files(root_path) {
    const fs_stat = fs.lstatSync(root_path);
    if (fs_stat.isFile()) {
        yield root_path;
    } else if (fs_stat.isDirectory()) {
        for (const sub_path of fs
            .readdirSync(root_path, { withFileTypes: true })
            .map((e) => e.name)) {
            yield* get_files(`${root_path}/${sub_path}`);
        }
    }
    yield;
}

async function main() {
    if (!config.append) fs.rmSync(config.output_path, { force: true });
    for (const p of get_files(config.input_sql_path)) {
        console.error(`-> ${p}`);
        await proc_file(p);
    }
}

main();
