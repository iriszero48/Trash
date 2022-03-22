import sqlite3
import typing


class IUploader:
    conn: sqlite3.Connection
    cur: sqlite3.Cursor

    def set_data_source(self, db_path: str, table: str, word: typing.List[str], order: typing.List[str], where: str,
                        *args) -> None:
        self.conn = sqlite3.connect(db_path, *args)
        self.cur = self.conn.cursor()
        self.cur.execute(f"select {', '.join(word)} from {table} {where} order by {', '.join(order)}")

    def walker(self,
               upload_callback: typing.Callable[[list], None],
               fail_callback: typing.Callable[[list, Exception], None]) -> None:
        while True:
            row = self.cur.fetchone()
            if row is None:
                break

            try:
                upload_callback(row)
            except Exception as e:
                fail_callback(row, e)
