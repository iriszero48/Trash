import typing

import pymssql
import pymysql
import pymongo

import config


@config.logger.catch
def move(
        src_keywords: typing.List[str], dest_keywords: typing.List[str],
        src_table: str, dest_table: str,
        order: str, desc: bool = False, take: int = 0, where: str = '', page: int = 100000, process = lambda x: x, do_insert: bool = True):
    src_conn = pymssql.connect(...)
    src_cur = src_conn.cursor()

    dst_conn = pymysql.connect(...)
    dst_cur = dst_conn.cursor()

    skip = 0
    done = False
    while not done:
        exec_sql = f"WITH Results_CTE AS (SELECT {', '.join(src_keywords)}, ROW_NUMBER() OVER (" \
                   f"ORDER BY {order} {'desc' if desc else ''}) AS RowNum FROM {src_table} {where}) " \
                   f"SELECT {', '.join(src_keywords)} FROM Results_CTE " \
                   f"WHERE RowNum >= {skip + 1} AND RowNum < {skip + page + 1}"
        config.logger.debug(exec_sql)
        src_cur.execute(exec_sql)
        data = [process(list(row)) for row in src_cur.fetchall()]
        data_size = len(data)
        if take != 0 and data_size + skip > take:
            data_size = take - skip
            data = data[:data_size]
        if data_size != page:
            done = True

        if do_insert:
            insert_sql = f"INSERT INTO {dest_table} ({', '.join(dest_keywords)}) " \
                         f"VALUES ({', '.join(['%s' for _ in range(len(dest_keywords))])})"
            config.logger.debug(insert_sql)
            dst_cur.executemany(insert_sql, data)

        skip += data_size

    config.logger.debug('commit.')
    dst_conn.commit()
    dst_conn.close()


@config.logger.catch
def move_to_mongodb(
        src_keywords: typing.List[str], dest_keywords: typing.List[str],
        src_table: str, dest_table: str,
        order: str, desc: bool = False, take: int = 0, where: str = '', page: int = 100000, process=lambda x: x):
    src_conn = pymssql.connect(...)
    src_cur = src_conn.cursor()

    dst_conn = pymongo.MongoClient(...)
    dst_db = dst_conn[...]
    dst_cur = dst_db[dest_table]

    skip = 0
    done = False
    while not done:
        exec_sql = f"WITH Results_CTE AS (SELECT {', '.join(src_keywords)}, ROW_NUMBER() OVER (" \
                   f"ORDER BY {order} {'desc' if desc else ''}) AS RowNum FROM {src_table} {where}) " \
                   f"SELECT {', '.join(src_keywords)} FROM Results_CTE " \
                   f"WHERE RowNum >= {skip + 1} AND RowNum < {skip + page + 1}"
        config.logger.debug(exec_sql)
        src_cur.execute(exec_sql)
        data = [process(list(row)) for row in src_cur.fetchall()]
        data_size = len(data)
        if take != 0 and data_size + skip > take:
            data_size = take - skip
            data = data[:data_size]
        if data_size != page:
            done = True

        data_json = []
        for row in data:
            row_json = {}
            for i in range(len(dest_keywords)):
                row_json[dest_keywords[i]] = row[i]
            data_json.append(row_json)

        dst_cur.insert_many(data_json)

        skip += data_size

    config.logger.debug('done.')
    dst_conn.close()
