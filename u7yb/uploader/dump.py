import sys
import platform
import argparse

import lib.dumper

from lib.dumper import IDumpable, SqliteDumper, MssqlDumper
from config import ms_url, logger


def dump(dumper: IDumpable, db_path: str, skip: int, take: int, remove: bool, only_available: bool):
    saver = lib.dumper.Saver(
        db_path,
        table,
        {},
        remove
    )
    dumper.set_execute_params([id_str, url_str, up_date_str], [id_str], table, where='' if only_available else '')
    dumper.set_skip_and_take(skip, take)
    logger.debug(dumper.get_sql_query())

    count = 0
    while True:
        row = dumper.fetchone()
        if row is None:
            break

        item = Item(row[0], row[1], row[2])
        saver.insert({id_str: item.id, url_str: item.url, up_date_str: item.up_date})
        count += 1

    logger.info(f'count: {count}')


@logger.catch
def run():
    if len(sys.argv) == 1:
        logger.info(f'\n{parser.format_help()}')
        exit(1)

    args = parser.parse_args()

    if platform.system() == 'Darwin':
        dumper = SqliteDumper('...')
    else:
        dumper = MssqlDumper(ms_url)

    dump(dumper, args.output, args.skip, args.take, args.overwrite, args.available)


if __name__ == '__main__':
    run()
