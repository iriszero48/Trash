import sqlite3
import json
import sys

if len(sys.argv) != 5:
    print(f"len(sys.argv) != 5 which len(sys.argv) = {len(sys.argv)}")
    print(f"Usage: {sys.argv[0]} sqlite table prefix output")
    exit(1)

sqlite_file = sys.argv[1]
table = sys.argv[2]
prefix = sys.argv[3]
out = sys.argv[4]

with open(out, 'wb') as fs:
    with sqlite3.Connection(sqlite_file) as conn:
        cur = conn.execute(f'select * from {table}')
        keys = [f'{prefix}_{i}' for i in next(zip(*cur.description))]
        for row in cur:
            v = json.dumps(
                {k: v for k, v in zip(keys, row)}, ensure_ascii=False)
            fs.write(f'{v}\n'.encode('utf-8'))
