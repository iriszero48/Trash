import json
import argparse
import sys
import os
import re
import typing

parser = argparse.ArgumentParser(description='sese_line_to_json')
parser.add_argument('-i', '--input', required=True, help="input path")
parser.add_argument('-o', '--output', default=None, help='output file path')
parser.add_argument('-p', '--prefix', required=True, help="key prefix")
parser.add_argument('-e', '--encoding', default=None,
                    help='input file encoding')
parser.add_argument('-f', '--format', default="{}_{}", help="key format")
parser.add_argument('--append', default=False,
                    action=argparse.BooleanOptionalAction)
parser.add_argument('--skip-row', default=0, type=int)
parser.add_argument('--ignore-error', default=None,
                    action='store_const', const='ignore', help="ignore error in read")
parser.add_argument('--cols', default=None, help="col names")
parser.add_argument('--types', default=None, help="col types")
parser.add_argument('--line-map', default="{}",
                    help="line maps (dict[str, list[str]])")

args = parser.parse_args()


def walker(path):
    if os.path.isfile(path):
        yield path
    elif os.path.isdir(path):
        for cwd, _, files in os.walk(path):
            for filename in files:
                yield os.path.join(cwd, filename)
    else:
        raise "unknow path attr"


class Keyword():
    type: str
    name: str

    def __init__(self, type, name) -> None:
        self.type = type
        self.name = name

    def dict(self) -> dict[str, str]:
        return {'type': self.type, 'name': self.name}


class Parser():
    def __init__(self, args) -> None:
        self.args = args

        self.skip_row_eta = args.skip_row
        self.line_map = eval(args.line_map)

        self.noting = False
        self.creating = False

        self.col_names = []

        self.skip_line = [
            re.compile(r"^DROP TABLE IF EXISTS \w+;$"),
            re.compile(r"^SET \w+[ ]*=[ ]*.*;$"),
            re.compile(r"^DROP TABLE IF EXISTS [`]{0,1}\w+[`]{0,1};$"),
            re.compile(r"^LOCK TABLES `\w*` WRITE;$"),
            re.compile(r"^UNLOCK TABLES;$"),
            re.compile(
                r"^ALTER (?:TABLE(?: ONLY){0,1}|SEQUENCE) [\w.]+ (?:(?:OWNER TO|OWNED BY) [\w.]+|ALTER COLUMN \w+ SET DEFAULT nextval\('[^']+'::\w+\));$"),
            re.compile(r"^SELECT .+;$")
        ]

        self.create_skip_line = [
            re.compile(
                r"^PRIMARY KEY[ ]*\((?:[`]{0,1}\w+[`]{0,1}[,]{0,1})*\)[,]{0,1}$"),
            re.compile(
                r"^UNIQUE KEY `\w+` \((?:`\w+`[,]{0,1})*\)(?: USING BTREE){0,1},{0,1}$"),
            re.compile(
                r"^KEY [`]{0,1}\w+[`]{0,1} \([`]{0,1}\w+[`]{0,1}(?:\(\d+\)){0,1}(,[`]{0,1}\w+[`]{0,1}(?:\(\d+\)){0,1}){0,}\)(?: USING BTREE){0,1}[,]{0,1}$"),
            re.compile(r"^FULLTEXT KEY `\w+` \(`\w+`\),{0,1}$")
        ]

        self.create_start_matcher = re.compile(
            r"^CREATE TABLE [`]{0,1}(\w+)[`]{0,1} \($")
        self.create_end_matcher = re.compile(
            r"^\)(?:| DEFAULT| \w*=[']{0,1}[^']*[']{0,1}){0,};$")
        self.create_col_name_matcher = re.compile(
            r"^[`]{0,1}(\w*)[`]{0,1} ((?:(?:medium|tiny|small|big|)int)|char|varchar|character|date|datetime|float|decimal|enum|timestamp|double|(?:|medium|long)text|(?:|medium)blob)(?: varying){0,1}(?:\(\d+(?:,\d+){0,1}\)|\('[^']+'(?:,'[^']+'){0,}\)|)(?: (?:unsigned|zerofill|NOT|NULL|auto_increment|character set utf8 collate (?:utf8_unicode_ci|utf8_general_mysql500_ci)|ON UPDATE CURRENT_TIMESTAMP|COMMENT (?:'[^']*')+|DEFAULT (?:'[^']*?'|NULL|CURRENT_TIMESTAMP))){0,},{0,1}$", re.IGNORECASE)

        self.noting_end_matcher = re.compile(r"^.*[*][/][;]*$")

        self.insert_string_matcher = r"'(?:[^']|\\')*'"
        self.insert_number_matcher = r"[+-]{0,1}\d+(?:\.\d+){0,1}"
        self.insert_null_matcher = r"(?:NULL|null)"
        self.insert_hex_matcher = r"0x[0-9A-F]+"
        self.insert_base_value_cond = lambda cap: rf"({'' if cap else '?:'}{self.insert_null_matcher}|{self.insert_number_matcher}|{self.insert_string_matcher}|{self.insert_hex_matcher})"
        self.insert_base_value_matcher = rf"(?:{self.insert_null_matcher}|{self.insert_number_matcher}|{self.insert_string_matcher}|{self.insert_hex_matcher})"
        self.insert_base_value_capturer = re.compile(
            rf"({self.insert_null_matcher}|{self.insert_number_matcher}|{self.insert_string_matcher}|{self.insert_hex_matcher})[,)]")
        self.insert_value_matcher = rf"\((?:{self.insert_base_value_matcher}[ ]*,[ ]*)*{self.insert_base_value_matcher}\)"
        self.insert_value_capturer = re.compile(
            rf"({self.insert_value_matcher})")
        self.insert_values_matcher = rf"(?:{self.insert_value_matcher}[ ]*,[ ]*)*{self.insert_value_matcher}"
        self.insert_matcher = re.compile(
            rf"^INSERT INTO [`]{{0,1}}[^`]+[`]{{0,1}}[ ]*VALUES[ ]*{self.insert_values_matcher};$")

        self.int_matcher = re.compile(r"^\d+$")
        self.float_matcher = re.compile(r"^\d+\.\d+$")

        self.create_othering = False
        self.create_other_begin_matcher = re.compile(
            r"^CREATE (SEQUENCE|INDEX) .+$")

        self.copy_mode = False
        self.copy_begin_matcher = re.compile(
            r"COPY \w+ \(\w+(?:,[ ]*\w+)*\) FROM stdin;")

        self.parse_idx = 0
        self.parse_max_length = 0
        self.parse_line = None

    def parse_token(self) -> str:
        tk = ''
        done = False
        while not done:
            ch = self.parse_line[self.parse_idx]
            assert ch.isascii()
            if ch.isspace():
                if len(tk) == 0:
                    pass
                else:
                    done = True
            elif ch.isalpha():
                tk += ch
            else:
                raise Exception("unknow token")

            self.parse_idx += 1
        return tk

    def parse_table_name(self) -> str:
        tab = ''
        done = False
        bk = None
        string = False
        esc = False
        while not done:
            ch = self.parse_line[self.parse_idx]
            if string:
                if esc:
                    assert 'wtf \\'
                elif ch == '\\':
                    esc = True
                elif ch == '`':
                    if bk is None:
                        raise Exception('unknow table name')
                    else:
                        string = False
                        done = True
                elif ch.isspace():
                    if bk is None:
                        string = False
                        done = True
                    else:
                        tab += ch
                else:
                    tab += ch
            else:
                if ch.isspace():
                    pass
                elif ch == '`':
                    bk = '`'
                    string = True
                elif ch.isalpha():
                    string = True
                    tab += ch
                else:
                    raise Exception('unknow table name')
            self.parse_idx += 1
        return tab

    def parse_number(self) -> typing.Union[float, int]:
        num = ''
        has_dot = False
        has_exp = False
        while True:
            ch = self.parse_line[self.parse_idx]
            if (len(num) == 0 or num[-1].lower() == 'e') and ch == '+' or ch == '-':
                num += ch
            elif ch.isdigit():
                num += ch
            elif ch == '.' and not has_dot and not has_exp:
                num += ch
                has_dot = True
            elif ch.lower() == 'e' and not has_exp:
                num += ch
                has_exp = True
            elif ch == ' ' or ch == ',' or ch == ')':
                break
            else:
                e = {'num': num, 'ch': ch, 'parse': self.parse_line[max(
                    0, self.parse_idx - 15):min(self.parse_max_length, self.parse_idx + 15)]}
                raise Exception(
                    f"unknow number: {json.dumps(e, ensure_ascii=False)}")

            self.parse_idx += 1
        return float(num) if has_dot or has_exp else int(num)

    def parse_null(self) -> None:
        assert self.parse_line[self.parse_idx:self.parse_idx +
                               4].lower() == 'null'
        self.parse_idx += 4
        ch = self.parse_line[self.parse_idx]
        if ch == ' ' or ch == ',' or ch == ')':
            return None
        else:
            raise Exception("unknow null")

    def parse_string(self) -> str:
        str_val = ''
        bk = None
        string = False
        esc = False
        # esc_val = ''
        while True:
            ch = self.parse_line[self.parse_idx]
            if string:
                if esc:
                    str_val += {'r': '\r',
                                'n': '\n',
                                't': '\t',
                                '"': '"',
                                "'": "'",
                                '\\': '\\',
                                'Z': '\x1a',
                                '0': '\0',
                                'b': '\b',
                                '%': '%',
                                '_': '_'}[ch]
                    esc = False
                elif ch == bk:
                    string = False
                elif ch == '\\':
                    esc = True
                else:
                    str_val += ch
            else:
                if ch == '`' or ch == '"' or ch == "'":
                    bk = ch
                    string = True
                elif ch.isspace():
                    pass
                elif ch == ',' or ch == ')':
                    return str_val
                else:
                    raise Exception("unknow string")

            self.parse_idx += 1

    def parse_blob(self) -> list[int]:
        assert self.parse_line[self.parse_idx:self.parse_idx + 2] == '0x'
        self.parse_idx += 2

        blob = ''
        while True:
            ch = self.parse_line[self.parse_idx]
            if (c := ch.lower(), ch.isdigit() or (c >= 'a' and c <= 'f'))[-1]:
                blob += ch
            elif ch == ' ' or ch == ',' or ch == ')':
                break
            else:
                raise Exception('unknow blob')

            self.parse_idx += 1
        return [i for i in bytes.fromhex(blob)]

    def parse_tuple(self) -> list[typing.Union[str, int, float, list[int], None]]:
        values = []
        done = False
        valing = False
        except_comman = False
        while not done:
            ch = self.parse_line[self.parse_idx]
            if ch.isspace():
                pass
            elif except_comman:
                if ch == ',':
                    except_comman = False
                elif ch == ')':
                    done = True
                else:
                    raise Exception('unknow value')
            else:
                if valing:
                    if ch == '0' and self.parse_line[self.parse_idx + 1] == 'x':
                        values.append(self.parse_blob())
                    elif ch == '+' or ch == '-' or ch.isdigit():
                        values.append(self.parse_number())
                    elif ch == 'n' or ch == 'N':
                        values.append(self.parse_null())
                    elif ch == '"' or ch == "'" or ch == '`':
                        values.append(self.parse_string())
                    elif ch == ')':
                        valing = False
                        done = True
                        return values
                    else:
                        raise Exception('unknow tuple value')
                    if valing:
                        except_comman = True
                    self.parse_idx -= 1
                else:
                    if ch == '(':
                        valing = True
                    else:
                        raise Exception("unknow values")
            self.parse_idx += 1
        return values

    def parse_insert(self, line) -> tuple[list[list[typing.Union[str, int, float, list[int], None]]], str, typing.Union[list[str], None]]:
        self.parse_idx = 0
        self.parse_max_length = len(line)
        self.parse_line = line

        assert self.parse_token() == 'INSERT'
        assert self.parse_token() == 'INTO'
        tab = self.parse_table_name()
        col_names = None
        assert self.parse_token() == 'VALUES'

        values = []
        except_comman = False
        while True:
            ch = self.parse_line[self.parse_idx]
            if ch.isspace():
                pass
            elif except_comman:
                if ch == ',':
                    except_comman = False
                elif ch == ';':
                    return values, tab, col_names
                else:
                    raise Exception("unknow insert")
            elif ch == '(':
                values.append(self.parse_tuple())
                except_comman = True
                self.parse_idx -= 1
            else:
                raise Exception("unknow insert")
            self.parse_idx += 1

    def auto_cast(self, cols: list[Keyword], vals: list[typing.Union[str, int, float, list[int], None]]) -> list[typing.Union[str, int, float, list[int], None]]:
        res = []
        for i in range(len(cols)):
            k = cols[i]
            if vals[i] is None:
                res.append(None)
            elif k.type.endswith('int'):
                res.append(int(vals[i]))
            elif k.type.endswith('blob'):
                if isinstance(vals[i], int):
                    vals[i] = [x for x in bytes.fromhex(hex(int(vals[i]))[2:])]
                elif isinstance(vals[i], str) and len(vals[i]) == 0:
                    res.append.append([])
                else:
                    raise Exception(
                        f'unknow blob: {json.dumps(vals[i], ensure_ascii=False)}')
            elif k.type.endswith('float') or k.type.endswith('double'):
                res.append(float(vals[i]))
            else:
                res.append(vals[i])
        return res

    def write_line(self, cols, vals) -> None:
        self.out.write(json.dumps({self.args.format.format(self.args.prefix, k.name): v for k, v in zip(
            cols, vals)}, ensure_ascii=False) + '\n')

    def proc_file(self, path) -> None:
        args = self.args

        with open(path, 'r', encoding=args.encoding, errors=args.ignore_error) as fs:
            for raw_line in fs:
                raw_line = raw_line.removesuffix(
                    '\r\n').removesuffix('\n').strip()
                for line in self.line_map[raw_line] if raw_line in self.line_map else [raw_line]:
                    if len(line) == 0:
                        continue

                    if line.startswith('/*'):
                        self.noting = True
                    if self.noting:
                        if self.noting_end_matcher.fullmatch(line):
                            self.noting = False
                        continue

                    if self.create_other_begin_matcher.fullmatch(line):
                        self.create_othering = True
                    if self.create_othering:
                        if line.endswith(';'):
                            self.create_othering = False
                        continue

                    if self.copy_begin_matcher.fullmatch(line):
                        self.copy_mode = True
                        continue
                    if self.copy_mode:
                        if line == r'\.':
                            self.copy_mode = False
                        else:
                            values = self.auto_cast(
                                self.col_names, line.split('\t'))
                            assert len(self.col_names) == len(values)
                            self.write_line(self.col_names, values)
                        continue

                    if self.create_start_matcher.fullmatch(line):
                        self.creating = True
                        self.col_names.clear()
                        continue
                    if self.creating:
                        if self.create_end_matcher.fullmatch(line):
                            self.creating = False
                            sys.stderr.write(json.dumps(
                                [i.dict() for i in self.col_names], ensure_ascii=False) + '\n')
                        elif any(map(lambda v: v.fullmatch(line), self.create_skip_line)):
                            pass
                        elif (r := self.create_col_name_matcher.findall(line), len(r) != 0)[-1]:
                            self.col_names.append(Keyword(r[0][1], r[0][0]))
                        else:
                            raise Exception(
                                f'unknow create line: {json.dumps(line, ensure_ascii=False)}')
                    elif line.startswith('--'):
                        continue
                    elif line.startswith('INSERT INTO'):
                        if self.skip_row_eta > 0:
                            self.skip_row_eta -= 1
                            continue
                        for col_values in self.parse_insert(line)[0]:
                            if len(self.col_names) != len(col_values):
                                raise Exception(
                                    f'assert len(self.col_names) == len(col_values): {json.dumps([i.dict() for i in self.col_names], ensure_ascii=False)} == {json.dumps(col_values, ensure_ascii=False)}')

                            self.write_line(
                                self.col_names, self.auto_cast(self.col_names, col_values))
                    elif any(map(lambda r: r.fullmatch(line), self.skip_line)):
                        continue
                    else:
                        raise Exception(
                            f'unknow line: {json.dumps(line, ensure_ascii=False)}')

    def run(self) -> None:
        args = self.args
        out = sys.stdout if args.output is None else open(
            args.output, 'a' if args.append else 'w', encoding='utf-8')
        self.out = out

        cn = []
        ct = []
        if args.cols is not None:
            cn = eval(args.cols)
        if args.types is not None:
            ct = eval(args.types)
        for t, n in zip(ct, cn):
            self.col_names.append(Keyword(t, n))
        try:
            for file_path in walker(args.input):
                sys.stderr.write(
                    f'-> {json.dumps(file_path, ensure_ascii=False)}\n')
                self.proc_file(file_path)
        finally:
            out.close()


Parser(args).run()
