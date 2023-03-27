import json
import pyodbc
import argparse
import re
import sys
import datetime
import getpass
import decimal

pyodbc.lowercase = False

parser = argparse.ArgumentParser(description='sese_line_to_json')
parser.add_argument('-o', '--output', default=None, help='output file path')
parser.add_argument('-p', '--prefix', required=True, help="key prefix")
parser.add_argument('-f', '--format', default="{}_{}", help="key format")
parser.add_argument('-t', '--table', required=True,
                    help="json {database: [table1, table2,...]}")
parser.add_argument('-d', '--driver', default="ODBC Driver 17 for SQL Server",
                    help=f"drivers: {pyodbc.drivers()}")
parser.add_argument(
    '-s', '--server', default="localhost", help="localhost or (local)\SQLEXPRESS")
parser.add_argument('-u', '--user', default="sa")
parser.add_argument('-a', '--append', default=False,
                    action=argparse.BooleanOptionalAction)

args = parser.parse_args()

sys.stderr.write(f"{args.__dict__}\n")


class datetime_encoder(json.JSONEncoder):
    def default(self, o):
        if isinstance(o, datetime.datetime):
            return o.isoformat()
        elif isinstance(o, decimal.Decimal):
            return str(o)
        elif isinstance(o, bytes):
            return str([int(i) for i in o])

        return super().default(o)


table_list = json.loads(args.table)

out = sys.stdout if args.output is None else open(
    args.output, 'a' if args.append else 'w', encoding='utf-8')
try:
    pw = getpass.getpass()
    for database in table_list:
        sys.stderr.write(f'-> {database}\n')
        conn = pyodbc.connect(
            rf'DRIVER={args.driver};'
            rf'SERVER={args.server};'
            r'Trusted_Connection=yes;'
            rf'Database={database};'
            rf'UID={args.user};'
            rf'PWD={pw};')
        cur = conn.cursor()

        if table_list[database] is None:
            cur.execute(
                "SELECT TABLE_CATALOG,TABLE_SCHEMA,TABLE_NAME FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_TYPE = 'BASE TABLE'")
            table_list[database] = [
                f'[{r[0]}].[{r[1]}].[{r[2]}]' for r in cur.fetchall()]

        for table_name in table_list[database]:
            sys.stderr.write(f'->> {table_name}\n')
            cur.execute(f'SELECT * FROM {table_name}')
            col_names = [args.format.format(
                args.prefix, desc[0]) for desc in cur.description]
            while row := cur.fetchone():
                out.write(json.dumps(
                    {col_name: row[idx] for col_name, idx in zip(col_names, range(len(col_names)))}, ensure_ascii=False, cls=datetime_encoder) + '\n')

        cur.close()
        conn.close()
finally:
    out.close()
