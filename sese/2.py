import json
import argparse
import sys
import os
import re

code_str = ''
name_str = ''
child_str = ''


def count_tab(string):
    idx = next(re.finditer(r'[^\t]', string)).start()
    if idx == -1:
        raise Exception("wtf")

    return idx


def clr_tree(root):
    if len(root[child_str]) == 0:
        del root[child_str]
    else:
        for c in root[child_str]:
            clr_tree(c)


with open('', 'w', encoding='utf-8') as out:
    curr_root = None
    curr_c1 = None
    curr_c2 = None
    curr_c3 = None
    curr_c4 = None
    # curr_level = 0
    tab_map = []

    with open('', 'r', encoding='ansi') as fs:
        for line in fs.readlines():
            code, name = [i.strip()
                          for i in line.strip('\t').strip().split('|')]

            tab = count_tab(line)

            # def save(): print(json.dumps(curr_root, ensure_ascii=False) + '\n')
            def save(): out.write(json.dumps(curr_root, ensure_ascii=False) + '\n')

            curr_level = tab_map.index(tab) if tab in tab_map else -1
            if curr_level == -1:
                tab_map.append(tab)
                curr_level = len(tab_map) - 1

            if curr_level == 0:
                if curr_root is not None:
                    clr_tree(curr_root)
                    save()
                    curr_root = None
                    tab_map = [0]

                curr_root = {code_str: code,
                             name_str: name, child_str: []}
                curr_c1 = curr_root[child_str]
            elif curr_level == 1:
                curr_c1.append({code_str: code, name_str: name, child_str: []})
                curr_c2 = curr_c1[-1][child_str]
            elif curr_level == 2:
                curr_c2.append({code_str: code, name_str: name, child_str: []})
                curr_c3 = curr_c2[-1][child_str]
            elif curr_level == 3:
                curr_c3.append({code_str: code, name_str: name, child_str: []})
                curr_c4 = curr_c3[-1][child_str]
            elif curr_level == 4:
                curr_c4.append({code_str: code, name_str: name, child_str: []})
            else:
                raise Exception("wtf")
    save()
