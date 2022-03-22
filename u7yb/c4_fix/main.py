import re
import time
import typing
import pathlib
import json
import functools

import requests

from loguru import logger
from bs4 import BeautifulSoup

import config

logger.add(
    sink=str(pathlib.Path(pathlib.Path.cwd(), 'log') / '{time}.log'),
    level='INFO',
    rotation='00:00',
    retention='30 days',
    compression='zip',
    encoding='utf-8',
    enqueue=True
)


class Url:
    @staticmethod
    def base_url():
        return config.base_url

    @staticmethod
    def index():
        return f'{Url.base_url()}/index.html'

    @staticmethod
    def get(sub_url: str):
        return f'{Url.base_url()}{pathlib.Path("/") / sub_url}'


class Item:
    id: str
    parent_id: str
    name: list[str]

    def __init__(self, id_: str, parent_id: str, name: list[str]):
        self.id = id_
        self.parent_id = parent_id
        self.name = name

    def __str__(self):
        return f"{{'id': '{self.id}', 'parent_id': '{self.parent_id}', 'name': '{'/'.join(self.name)}'}}"


def req_get(url: str):
    resp = requests.get(url)
    resp.encoding = 'utf-8'
    return resp


def get_area_list() -> dict[str, str]:
    res = {}
    for i in BeautifulSoup(req_get(Url.base_url()).text, features="html.parser").table.tr.find_all('a'):
        res[i.string] = i.get('href')
    return res


def get_all_date_impl(url: str, pre_id: str, pre_name: list[str]) -> typing.Generator[Item, None, None]:
    logger.info(f'visit "{url}"')
    for tr in BeautifulSoup(req_get(url).text, features="html.parser").table.tbody.find_all('tr'):
        item = list(tr.find_all('a'))
        code = item[0].string
        name = pre_name + [item[1].string]
        logger.info(f'fetch {code} <==> {"/".join(name)}')
        yield Item(code, pre_id, name)
        url = item[1].get('href')
        if url:
            for sub_item in get_all_date_impl(Url.get(url), code, name + []):
                yield sub_item


def get_all_date(areas: dict[str, str]) -> typing.Generator[Item, None, None]:
    for area, url in areas.items():
        item_id = url[2:].split('.')[0]
        logger.info(f'visit {item_id} {area} "{url}"')
        yield Item(item_id, '0', [area])
        sub_url = Url.get(url)
        for item in get_all_date_impl(sub_url, item_id, [area]):
            yield item


def insert_data(items: typing.Iterable[Item]):
    count = 0
    for item in items:
        ret = config.db_cur.execute(
            "INSERT INTO c4.c4_area (id, parentid, name, customers) VALUES (%s, %s, %s, DEFAULT)",
            [item.id, item.parent_id, '/'.join(item.name)]
        )
        logger.info(f'insert {item}, return {ret}')
        count += 1
    config.db.commit()
    logger.info(f'{count} row affected')


def update_data(path: str):
    count = 0

    with open(path, 'r') as f:
        for line in f.readlines():
            name, area_raw = line.split()
            areas = area_raw.replace('\\', '/').split('/')
            for level in range(len(areas)):
                cur_area = '/'.join(areas[:level + 1])
                ret = config.db_cur.execute(
                    "UPDATE c4.c4_area t SET t.customers = t.customers + 1 where t.name = %s",
                    [cur_area]
                )
                if ret == 0:
                    logger.error(f'update "{cur_area}" failed')
                count += 1
            logger.info(f'update {area_raw}')

    config.db.commit()
    logger.info(f'{count} row affected')


def query(op):
    try:
        logger.info(f'processing {op}')

        arg = '0'
        if not (len(op) == 1 or len(op) == 2):
            raise Exception('error splitting the argument list: invalid argument')
        if op[0] != 'area-list':
            raise Exception('invalid command found when processing input')
        if len(op) == 2 and re.fullmatch(r"^\d+$", op[1]):
            arg = op[1]

        config.db_cur.execute(
            "select id, name, parentid from c4_area where parentid = %s order by id",
            [arg]
        )
        data = []
        while True:
            row = config.db_cur.fetchone()
            if not row:
                break

            data.append({'id': row[0], 'name': row[1].replace('/', '\\'), 'parentid': row[2]})
        return {
            'ServerTime': int(time.time()),
            'ServerNo': 'SN000',
            'date': data
        }

    except Exception as ex:
        logger.error(f'command "{" ".join(op)}" error: {ex}')
        return {
            'ServerTime': int(time.time()),
            'ServerNo': 'SN200',
            'date': []
        }


def interactive():
    while True:
        op = input().split()
        if len(op) == 0:
            logger.info('exit')
            break
        else:
            data = query(op)
            logger.info(f'return {json.dumps(data, ensure_ascii=False)}')
            print(json.dumps(data, indent=4))


def func_combine(*func):
    return lambda *args: functools.reduce(lambda s, x: x(s), func[1:], func[0](*args))


@logger.catch
def run():
    logger.info('fetch data ...')
    func_combine(get_area_list, get_all_date, insert_data)()

    logger.info('update data ...'),
    update_data(config.data_path)

    logger.info('interactive ...'),
    interactive()


if __name__ == '__main__':
    run()
