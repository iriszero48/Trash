import json
import argparse
import sys
import os
import re
import itertools

parser = argparse.ArgumentParser(description='sese_line_to_json')
parser.add_argument('-i', '--input', required=True, help="input path")
parser.add_argument('-o', '--output', default=None, help='output file path')
parser.add_argument('-p', '--prefix', required=True, help="key prefix")
parser.add_argument('-e', '--encoding', default=None,
                    help='input file encoding')
parser.add_argument('-s', '--split', default=None,
                    help="split token(python str \"{token}\")")
parser.add_argument('-c', '--col', required=True,
                    help="col names(python string array)")
parser.add_argument('--format', default="{}_{}", help="key format")
parser.add_argument('--ignore-error', default=None,
                    action='store_const', const='ignore', help="ignore error in read")
parser.add_argument('--auto-fill-null', default=False,
                    action='store_const', const=True, help="auto fill null")
parser.add_argument('--auto-combine-head', default=False,
                    action='store_const', const=True, help="auto combine head col")
parser.add_argument('--auto-combine-tail', default=False,
                    action='store_const', const=True, help="auto combine tail col")
parser.add_argument('--combine-col', default=None, type=int,
                    metavar=int, help="combine col spec")
parser.add_argument('-v', '--valid', default=None,
                    help="valid regex value(python string array)")
parser.add_argument('-n', '--nullable', default=None,
                    help="nullable value(json bool array)")
parser.add_argument('--replace-empty', default=False, action='store_const', const=True,
                    help="replace empty string to null")
parser.add_argument('--disable-print-fill-info',
                    action=argparse.BooleanOptionalAction)
parser.add_argument('--remove-tail-empty',
                    action=argparse.BooleanOptionalAction)
parser.add_argument('--skip-line', default=0, type=int, metavar=int)
parser.add_argument('--intervate', default=False,
                    action=argparse.BooleanOptionalAction)
parser.add_argument('--maps', default=None, help="(string? -> string)[]")
parser.add_argument('--append', default=False,
                    action=argparse.BooleanOptionalAction)
parser.add_argument('--line-map', default=None, help="string -> string")

args = parser.parse_args()
sys.stderr.write(f'{args.__dict__}\n')

assert not (args.auto_combine_head and args.auto_combine_tail)

value_maps = None if args.maps is None else eval(args.maps)
line_map = None if args.line_map is None else eval(args.line_map)


def walker(path):
    if os.path.isfile(path):
        yield path
    elif os.path.isdir(path):
        for cwd, _, files in os.walk(path):
            for filename in files:
                yield os.path.join(cwd, filename)
    else:
        raise "unknow path attr"


split_token = eval(f'"{args.split}"') if args.split else None

out = sys.stdout if args.output is None else open(
    args.output, 'a' if args.append else 'w', encoding='utf-8')
try:
    for file_path in walker(args.input):
        sys.stderr.write(f'-> {file_path}\n')
        with open(file_path, 'r', encoding=args.encoding, errors=args.ignore_error) as fs:
            col_names = [args.format.format(
                args.prefix, col_name) for col_name in eval(args.col)]
            col_valid = [re.compile(r) for r in eval(
                args.valid)] if args.valid is not None else None
            col_nullable = json.loads(
                args.nullable) if args.nullable is not None else None
            for row in itertools.islice(map(lambda line: line.removesuffix('\r\n').removesuffix('\n'), fs), args.skip_line, None):
                if len(row) == 0:
                    continue

                if line_map:
                    row = line_map(row)

                row = row.split(split_token)
                if len(row) == 0:
                    continue

                if args.remove_tail_empty:
                    while len(row[-1]) == 0:
                        row.pop()

                if args.replace_empty:
                    row = [None if len(c) == 0 else c for c in row]

                try:
                    if len(row) != len(col_names):
                        if len(row) < len(col_names) and args.auto_fill_null:
                            if not args.disable_print_fill_info:
                                sys.stderr.write(f'... <= {row}\n')
                            row += [None] * (len(col_names) - len(row))
                            if not args.disable_print_fill_info:
                                sys.stderr.write(f'... => {row}\n')
                        elif len(row) > len(col_names) and (args.auto_combine_head or args.auto_combine_tail or args.combine_col):
                            sys.stderr.write(f'... <= {row}\n')
                            if args.auto_combine_head:
                                row = [(split_token if split_token else ' ').join(row[:len(row) - len(col_names) + 1])] + \
                                    row[len(row) - len(col_names) + 1:]
                            elif args.auto_combine_tail:
                                row = row[:len(col_names) - 1] + [
                                    (split_token if split_token else ' ').join(row[len(col_names) - 1:])]
                            else:
                                row = row[:args.combine_col] + \
                                    [(split_token if split_token else ' ').join(
                                        row[args.combine_col:args.combine_col+len(row) - len(col_names)])] + row[args.combine_col+len(row) - len(col_names):]
                            sys.stderr.write(f'... => {row}\n')
                        else:
                            sys.stderr.write(f'... <= {row}\n')
                            assert len(row) == len(col_names)

                    if col_nullable is not None:
                        ok = all([False if row[i] is None and not col_nullable[i]
                                  else True for i in range(len(row))])
                        if not ok:
                            sys.stderr.write(
                                f'... (!!) {row} <?> {args.nullable}\n')
                        assert ok

                    if value_maps:
                        row = [f(v) for v, f in zip(row, value_maps)]

                    if col_valid is not None:
                        ok = all([re.match(col_valid[i], row[i])
                                  for i in range(len(row)) if row[i] is not None])
                        if not ok:
                            sys.stderr.write(
                                f'... (!!) {row} <|> {eval(args.valid)}\n')
                        assert ok
                except Exception as e:
                    sys.stderr.write(f'{e}\n')
                    if args.intervate:
                        row = []
                        for col_name in col_names:
                            while True:
                                raw_value = input(
                                    f'{json.dumps(col_name, ensure_ascii=False)} > ')
                                if len(raw_value) == 0:
                                    continue

                                try:
                                    value = eval(f'"raw_value"')
                                    row.append(value)
                                    break
                                except Exception as e:
                                    sys.stderr.write(f'{e}\n')
                    else:
                        raise e

                out.write(json.dumps({k: v for k, v in zip(
                    col_names, row)}, ensure_ascii=False) + '\n')


finally:
    out.close()
