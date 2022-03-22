import os.path
import sqlite3
import typing
from abc import ABCMeta, abstractmethod

import pyodbc


class IDumpable():
    __metaclass__ = ABCMeta

    executed: bool

    word: typing.List[str]
    order: typing.List[str]
    table: str
    where: str
    skip: int = 0
    take: int = 0

    @abstractmethod
    def __init__(self, db, *args) -> None:
        pass

    @abstractmethod
    def execute_impl(self, sql: str) -> None:
        pass

    @abstractmethod
    def fetchone_impl(self) -> typing.Optional[list]:
        pass

    def get_sql_query(self) -> str:
        words = ', '.join(self.word)
        orders = ', '.join(self.order)
        take = '~0' if self.take == 0 else f'{self.skip + self.take + 1}'
        return f"select {words} from {self.table} {self.where} order by {orders} limit {take} offset {self.skip}"

    def set_execute_params(self, word: typing.List[str], order: typing.List[str], table: str, where: str = '') -> None:
        self.executed = False
        self.skip, self.take = 0, 0
        self.word, self.order, self.table, self.where = word, order, table, where

    def set_skip_and_take(self, skip: int = 0, take: int = 0) -> None:
        self.executed = False
        self.skip, self.take = skip, take

    def execute(self) -> None:
        sql = self.get_sql_query()
        self.execute_impl(sql)

    def fetchone(self) -> typing.Optional[list]:
        if not self.executed:
            self.execute()
            self.executed = True
        return self.fetchone_impl()


class MssqlDumper(IDumpable):
    conn: pyodbc.Connection
    cur: pyodbc.Cursor

    def __init__(self, p_str, *args):
        self.conn = pyodbc.connect(p_str, *args)
        self.cur = self.conn.cursor()

    def get_sql_query(self) -> str:
        words = ', '.join(self.word)
        orders = ', '.join(self.order)
        take = '' if self.take == 0 else f'AND RowNum < {self.skip + self.take + 1}'
        return f"WITH Results_CTE AS (SELECT {words}, ROW_NUMBER() OVER (ORDER BY " \
               f"{orders}) AS RowNum FROM {self.table} {self.where}) SELECT {words} FROM" \
               f" Results_CTE WHERE RowNum >= {self.skip + 1} {take}".strip()

    def execute_impl(self, sql) -> None:
        self.cur.execute(sql)

    def fetchone_impl(self) -> typing.Optional[pyodbc.Row]:
        return self.cur.fetchone()

    def __del__(self):
        self.conn.close()


class SqliteDumper(IDumpable):
    conn: sqlite3.Connection
    cur: sqlite3.Cursor

    def __init__(self, path, *args):
        self.conn = sqlite3.connect(path, *args)
        self.cur = self.conn.cursor()

    def execute_impl(self, sql) -> None:
        self.cur.execute(sql)

    def fetchone_impl(self) -> typing.Optional[list]:
        return self.cur.fetchone()

    def __del__(self):
        self.conn.close()


class Saver:
    table: str
    table_cols: typing.Dict[str, str]

    conn: sqlite3.Connection
    cur: sqlite3.Cursor

    def __init__(self, db_path: str, table: str, table_cols: typing.Dict[str, str], remove_if_exists: bool = False, *args):
        self.table = table
        self.table_cols = table_cols

        if remove_if_exists and os.path.exists(db_path):
            os.remove(db_path)

        if not os.path.exists(db_path):
            conn = sqlite3.connect(db_path)
            cur = conn.cursor()
            table_desc = ', '.join([f'{word} {desc}' for word, desc in self.table_cols.items()])
            cur.execute(f"create table items ({table_desc})")
            conn.commit()
            conn.close()

        self.conn = sqlite3.connect(db_path, *args)
        self.cur = self.conn.cursor()

    def insert(self, values: typing.Dict[str, str]):
        words = []
        data = []
        for w, v in values.items():
            words.append(w)
            data.append(v)

        self.cur.execute(
            f"insert into {self.table} ({', '.join(words)}) values ({', '.join(['?' for _ in data])})",
            data
        )

    def close(self):
        self.conn.commit()
        self.conn.close()

    def __del__(self):
        try:
            self.close()
        except:
            pass
