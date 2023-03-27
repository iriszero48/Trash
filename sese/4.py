import json
import os
import sys

base_path = ''
with open('', 'w', encoding='utf-8') as out:
    fp = os.path.join(base_path, '')
    sys.stderr.write(f'-> {fp}\n')
    with open(fp, 'r', encoding='utf-8') as fs:
        for line in filter(lambda l: len(l) != 0, map(lambda l: l.removesuffix('\r\n').removesuffix('\n'), fs)):
            out.write(json.dumps({'': line}, ensure_ascii=False) + '\n')

    for cwd, _, files in os.walk(os.path.join(base_path, '')):
        dir = os.path.basename(cwd)
        find_pos = [dir.find('('), dir.find('（')]
        dir = dir[:max(find_pos)]
        for filename in files:
            fp = os.path.join(cwd, filename)
            sys.stderr.write(f'-> {fp}\n')
            with open(fp, 'r', encoding='ansi') as fs:
                for line in filter(lambda l: len(l) != 0, map(lambda l: l.removesuffix('\r\n').removesuffix('\n'), fs)):
                    out.write(json.dumps(
                        {'': line, '': dir}, ensure_ascii=False) + '\n')
