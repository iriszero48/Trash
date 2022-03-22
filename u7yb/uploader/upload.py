import argparse
import datetime
import os.path
import pathlib
import sys
import time
import typing

import csv
import _helper
from config import oss_bucket, logger
from lib.oss_uploader import OssUploader, OssFileItem, func_combine
from lib.utility import CsvReader, CsvWriter


class OssCsv:
    id_set: typing.Set[int]
    csv_file: typing.TextIO
    writer = None
    cur: int

    def __init__(self, path: str):
        self.id_set = set()
        if os.path.exists(path):
            with open(path, 'r', newline='') as f:
                reader = csv.reader(f)
                for row in reader:
                    self.id_set.add(int(row[0]))
        self.csv_file = open(path, 'a', newline='')
        self.writer = csv.writer(self.csv_file)

    def assign_id_set(self, skips: typing.Iterable[int]) -> None:
        for i in skips:
            self.id_set.add(i)

    def exists(self, item_id: int) -> bool:
        return item_id in self.id_set

    def write(self, item_id: int, oss_path: str) -> None:
        self.writer.writerow([item_id, oss_path])
        self.id_set.add(item_id)
        self.csv_file.flush()

    def __del__(self):
        self.csv_file.close()


class UploadCursor:
    path: str

    def __init__(self, path: str):
        self.path = path

    def write(self, cur: int) -> None:
        with open(self.path, 'w', newline='') as fs:
            fs.write(str(cur))

    def read(self) -> int:
        if not os.path.exists(self.path):
            return 0
        with open(self.path, 'r') as fs:
            return int(fs.read())


class Upload(OssUploader):
    total_file_num: int = 0
    start_time: time
    total_file_size: int = 0

    def __init__(self, db_path: str, skips: typing.Optional[str]):
        super().__init__(oss_bucket)
        db_path_stem = pathlib.Path(db_path).stem
        self.total_file_num = len(self.oss_csv.id_set)
        if skips is not None:
            self.oss_csv.assign_id_set([int(i[0]) for i in CsvReader(skips).items()])

    def log_fail(self, row: list, ex: Exception) -> None:
        self.err_csv.writerow([row, str(ex)])

    def process(self, row: list) -> typing.Optional[OssFileItem]:
        item = Item(row[0], row[1], datetime.datetime.strptime(row[2].split('.')[0], "%Y-%m-%d %H:%M:%S"))
        if self.oss_csv.exists(item.id):
            return None

        self.cur__item = item

        path = _helper.get__real_path(self.machine, item.url)
        oss_item = OssFileItem(_helper.generate_oss_key(item, path), path)

        return oss_item

    def save(self, oss_item: OssFileItem) -> None:
        self.oss_csv.write(self.cur__item.id, oss_item.key)
        self.suc_cur.write(self.cur__item.id)
        self.total_file_num += 1
        self.total_file_size += os.path.getsize(oss_item.file_path)

    def run(self) -> None:
        self.start_time = time.time()
        self.walker(func_combine(
            self.process,
            self.oss_upload,
            self.save,
        ), self.log_fail)


@logger.catch
def run():
    if len(sys.argv) == 1:
        logger.info(f'\n{parser.format_help()}')
        exit(1)

    args = parser.parse_args()

    uploader = Upload(
        args.database,
        args.skips,
    )
    uploader.run()


if __name__ == '__main__':
    run()
