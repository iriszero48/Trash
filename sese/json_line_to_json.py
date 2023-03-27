import json
import argparse
import sys
import os
import bson
from bson import json_util

parser = argparse.ArgumentParser(description='sese_line_to_json')
parser.add_argument('-i', '--input', required=True, help="input path")
parser.add_argument('-o', '--output', default=None, help='output file path')
parser.add_argument('-p', '--prefix', required=True, help="key prefix")
parser.add_argument('-e', '--encoding', default=None,
                    help='input file encoding')
parser.add_argument('-f', '--format', default="{}_原始值", help="key format")
parser.add_argument('--ignore-error', default=None,
                    action='store_const', const='ignore', help="ignore error in read")
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


def proc_file(file_path):
    if file_path.endswith('.json'):
        with open(file_path, 'r', encoding=args.encoding, errors=args.ignore_error) as fs:
            lines = filter(lambda l: len(l) != 0, map(
                lambda l: l.removesuffix('\r\n').removesuffix('\n'), fs))
            for line in lines:
                yield json.loads(line)
    elif file_path.endswith('.bson'):
        with open(file_path, 'rb') as fs:
            for line in bson.decode_file_iter(fs):
                yield json.loads(json_util.dumps(line))
    else:
        raise Exception('unknow file ext')
    return


out = sys.stdout if args.output is None else open(
    args.output, 'a' if args.append else 'w', encoding='utf-8')
try:
    key = args.format.format(args.prefix)
    for file_path in walker(args.input):
        sys.stderr.write(f'-> {file_path}\n')
        for data in proc_file(file_path):
            out.write(json.dumps({key: data}, ensure_ascii=False) + '\n')
finally:
    out.close()
