import argparse
import pandas
import json
import os
import sys

parser = argparse.ArgumentParser(description='sese_excel_to_json')
parser.add_argument('-i', '--input', required=True, help="input path")
parser.add_argument('-o', '--output', default=None, help='output file path')
parser.add_argument('-p', '--prefix', required=True, help="key prefix")
parser.add_argument('--title', default=True,
                    action=argparse.BooleanOptionalAction)
parser.add_argument('--append', default=False,
                    action=argparse.BooleanOptionalAction)

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


out = sys.stdout if args.output is None else open(
    args.output, 'a' if args.append else 'w', encoding='utf-8')
try:
    for file_path in walker(args.input):
        sys.stderr.write(f'-> {file_path}\n')
        with pandas.ExcelFile(file_path) as ef:
            for sheet_name in ef.sheet_names:
                sys.stderr.write(f'->> {sheet_name}\n')
                df = ef.parse(sheet_name, header=0 if args.title else None)
                for _, row in df.iterrows():
                    row_dict = json.loads(row.to_json(force_ascii=False))
                    out.write(
                        f"{json.dumps({f'{args.prefix}_{key}': row_dict[key] for key in row_dict}, ensure_ascii=False)}\n")
finally:
    out.close()
