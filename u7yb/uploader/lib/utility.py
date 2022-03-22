import csv
import typing


def open_csv_reader(path: str, mode: str = 'r') -> typing.Tuple[typing.TextIO, typing.Any]:
    fs = open(path, mode, newline='', encoding='utf-8')
    reader = csv.reader(fs)
    return fs, reader


def open_csv_writer(path: str, mode: str = 'a') -> typing.Tuple[typing.TextIO, typing.Any]:
    fs = open(path, mode, newline='', encoding='utf-8')
    writer = csv.writer(fs)
    return fs, writer


class CsvReader:
    fs: typing.TextIO
    reader: typing.Any

    closed: bool = False

    def __init__(self, path: str, mode: str = 'r'):
        self.fs, self.reader = open_csv_reader(path, mode)
        self.closed = False

    def items(self) -> list:
        return list(self.reader)

    def close(self) -> None:
        self.fs.close()
        self.closed = True

    def __del__(self):
        if not self.closed:
            self.close()


class CsvWriter:
    fs: typing.TextIO
    writer: typing.Any

    closed: bool = True

    def __init__(self, path: str, mode: str = 'a'):
        self.fs, self.writer = open_csv_writer(path, mode)
        self.closed = False

    def writerow(self, row: typing.Iterable) -> None:
        self.writer.writerow(row)

    def close(self):
        self.fs.close()
        self.closed = True

    def __del__(self):
        if not self.closed:
            self.close()
