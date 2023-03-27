import json
import os
import sys

from lxml import html

prefix = ''
key_prefix = ''

with open('', 'w', encoding='utf-8') as out:
    def walk_files(cur_path):
        print(f"-> cd {cur_path}")
        for filename in os.listdir(cur_path):
            file_path = os.path.join(cur_path, filename)
            if os.path.isdir(file_path):
                walk_files(file_path)
            elif os.path.isfile(file_path):
                print(f'-> cat {filename}')

                if cur_path.endswith(""):
                    if filename.endswith('.html'):
                        with open(file_path, 'r', encoding='utf-8') as fs:
                            trs = iter(html.fromstring(
                                fs.read()).xpath("/html/body/table/tr"))
                            title = [
                                f"{key_prefix}_{td.text.strip()}" for td in next(trs)]
                            for tr in trs:
                                out.write(
                                    f"{json.dumps(dict(zip(title, [td.text for td in tr])), ensure_ascii=False)}\n")
                    else:
                        raise "unknow file ext"
                else:
                    if filename.endswith('.txt'):
                        key = f"{key_prefix}_{file_path.removeprefix(prefix)}_原始值".replace(
                            '\\', '/')
                        with open(file_path, 'rb') as fs:
                            for line in fs:
                                try:
                                    line = line.decode('utf-8')
                                except UnicodeDecodeError as ex:
                                    print(
                                        f"warning: error in line \"{line}\": {ex}", file=sys.stderr)
                                    line = line.decode('utf-8', 'ignore')
                                line = line.removesuffix(
                                    '\r\n').removesuffix('\n')
                                if len(line) != 0:
                                    out.write(
                                        f"{json.dumps({key: line}, ensure_ascii=False)}\n")
                    else:
                        raise "unknow file ext"
            else:
                raise "unknow file attr"

    walk_files(prefix)
