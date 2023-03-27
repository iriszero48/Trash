import json
import pyodbc
import os
import argparse
import sys
import datetime

pyodbc.lowercase = False

parser = argparse.ArgumentParser(description='sese_mdb_to_json')
parser.add_argument('-i', '--input', required=True, help="input path")
parser.add_argument('-o', '--output', required=True, help='output file path')
parser.add_argument('-p', '--prefix', required=True, help="key prefix")

args = parser.parse_args()

black_list = {
    'MSysAccessStorage',
    'MSysACEs',
    'MSysComplexColumns',
    'MSysIMEXColumns',
    'MSysIMEXSpecs',
    'MSysNavPaneGroupCategories',
    'MSysNavPaneGroups',
    'MSysNavPaneGroupToObjects',
    'MSysNavPaneObjectIDs',
    'MSysObjects',
    'MSysQueries',
    'MSysRelationships'
}


def walker(path):
    if os.path.isfile(path):
        yield path
    elif os.path.isdir(path):
        for cwd, _, files in os.walk(path):
            for filename in files:
                yield os.path.join(cwd, filename)
    else:
        raise "unknow path attr"


class datetime_encoder(json.JSONEncoder):
    def default(self, o):
        if isinstance(o, datetime.datetime):
            return o.isoformat()

        return super().default(o)


with open(args.output, 'w', encoding='utf-8') as out:
    for file_path in walker(args.input):
        sys.stderr.write(f'-> {file_path}\n')
        conn = pyodbc.connect(
            f"Driver={{Microsoft Access Driver (*.mdb, *.accdb)}};Dbq={file_path};")
        cur = conn.cursor()

        for table_name in [table.table_name for table in cur.tables() if table.table_name not in black_list]:
            sys.stderr.write(f'->> {table_name}\n')
            cur.execute(f'SELECT * FROM {table_name}')
            col_names = [desc[0] for desc in cur.description]
            while True:
                row = cur.fetchone()
                if row is None:
                    break

                out.write(json.dumps(
                    {f'{args.prefix}_{col_name}': row[idx] for col_name, idx in zip(col_names, range(len(col_names)))}, ensure_ascii=False, cls=datetime_encoder) + '\n')

        cur.close()
        conn.close()
