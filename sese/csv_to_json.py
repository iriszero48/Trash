import argparse
import csv
import json
import os
import sys

parser = argparse.ArgumentParser(description='sese_excel_to_json')
parser.add_argument('-i', '--input', required=True, help="input path")
parser.add_argument('-o', '--output', default=None, help='output file path')
parser.add_argument('-p', '--prefix', required=True, help="key prefix")
parser.add_argument('-e', '--encoding', default=None, help="file encoding")
parser.add_argument('-t', '--types', default=None, help="string[]")
parser.add_argument('--title', default=True,
                    action=argparse.BooleanOptionalAction)
parser.add_argument('--append', default=False,
                    action=argparse.BooleanOptionalAction)

args = parser.parse_args()

value_types = None if args.types is None else eval(args.types)


def walker(path):
    if os.path.isfile(path):
        yield path
    elif os.path.isdir(path):
        for cwd, _, files in os.walk(path):
            for filename in files:
                yield os.path.join(cwd, filename)
    else:
        raise "unknow path attr"


out = sys.stdout if args.output is None else open(
    args.output, 'a' if args.append else 'w', encoding='utf-8')
try:
    for file_path in walker(args.input):
        sys.stderr.write(f'-> {file_path}\n')
        with open(file_path, encoding=args.encoding, newline="\n") as fs:
            df = csv.reader(fs, delimiter=",", quotechar='"')
            if args.title:
                title = next(df)
            else:
                assert False

            title_num = len(title)
            for row in df:
                row_num = len(row)
                if row_num > title_num:
                    title += [f'undefined.{title_num+i}' for i in range(
                        row_num - title_num)]
                    title_num = len(title)
                None if len(row) == 0 else out.write(json.dumps({f'{args.prefix}_{title[i]}': (
                    row[i] if value_types is None else value_types[i](row[i])) for i in range(len(row))}, ensure_ascii=False) + '\n')
finally:
    out.close()
