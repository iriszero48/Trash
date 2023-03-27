import json
import argparse
import sys
import os

parser = argparse.ArgumentParser(description='sese_line_to_json')
parser.add_argument('-i', '--input', required=True, help="input path")
parser.add_argument('-o', '--output', default=None, help='output file path')
parser.add_argument('-p', '--prefix', required=True, help="key prefix")
parser.add_argument('-e', '--encoding', default=None,
                    help='input file encoding')
parser.add_argument('-f', '--format', default="{}_{}", help="key format")
parser.add_argument('--ignore-error', default=None,
                    action='store_const', const='ignore', help="ignore error in read")
parser.add_argument('--skip-space-line', const=True,
                    default=False, action='store_const', help="skip only space line")
parser.add_argument('--append', default=False,
                    action=argparse.BooleanOptionalAction)
parser.add_argument('--map-filepath-as', default=None,
                    help="return (filename:str) -> (col_name:str, value:any)")
parser.add_argument('--map', default=None, help="(value:str) -> (value:str)")

args = parser.parse_args()

map_filepath_func = None
if args.map_filepath_as is not None:
    map_filepath_func = eval(args.map_filepath_as)

map_func = None
if args.map is not None:
    map_func = eval(args.map)


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
        key_raw = args.format.format(args.prefix, '原始值')
        with open(file_path, 'r', encoding=args.encoding, errors=args.ignore_error) as fs:
            lines = filter(lambda l: not (args.skip_space_line and len(l.split()) == 0), filter(
                lambda l: len(l) != 0, map(lambda l: l.removesuffix('\r\n').removesuffix('\n'), fs)))
            if map_func:
                lines = map(map_func, lines)
            if map_filepath_func:
                for line in lines:
                    col_name, col_val = map_filepath_func(file_path)
                    out.write(json.dumps(
                        {key_raw: line, args.format.format(args.prefix, col_name): col_val}, ensure_ascii=False) + '\n')
            else:
                for line in lines:
                    out.write(json.dumps(
                        {key_raw: line}, ensure_ascii=False) + '\n')
finally:
    out.close()
