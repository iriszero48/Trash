import glob
import json
import os
import pathlib
import typing

import requests

from loguru import logger


logger.add(sink=str(pathlib.Path(pathlib.Path.cwd(), 'log') / '{time}.log'),
           rotation='00:00',
           retention='30 days',
           encoding='utf-8',
           enqueue=True,
           )


class LineDatabase:
    line_set: typing.Set[str]
    file: typing.TextIO

    def __init__(self, path: str):
        self.line_set = set()
        if os.path.exists(path):
            with open(path, 'r') as f:
                for line in f.readlines():
                    self.line_set.add(line.strip())
        self.file = open(path, 'a', newline='')

    def exists(self, line: str) -> bool:
        return line in self.line_set

    def write(self, line: str) -> None:
        self.file.writelines([line])
        self.file.flush()

    def __del__(self):
        self.file.close()


def proc(url):
    logger.info(f'GET {url}')
    resp = requests.get(url)
    logger.info(f'{resp.status_code} {resp.text}')
    resp = json.loads(resp.text)
    requests.get(vid_url)


@logger.catch
def main():
    db = LineDatabase("")
    for json_file in glob.glob(""):
        with open(json_file, 'r') as json_fs:
            for ids in json.loads(json_fs.read())["data"]:
                if db.exists(f'{l_id}|{c_id}'):
                    continue
                try:
                    proc(url)
                except Exception as ex:
                    logger.error(json.dumps({"id": json.dumps(ids), "err": f'{ex}', "url": url}))


if __name__ == '__main__':
    main()
