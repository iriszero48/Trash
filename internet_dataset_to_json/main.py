import json
import struct
import brotli

def _load1(b: bytes):
    n = struct.unpack('i', b[:4])[0]
    字符串长度 = struct.unpack(f'{n}h', b[4:4+n*2])
    吸0 = struct.unpack(f'{n}e', b[4+n*2:4+n*4])
    文hint = ''.join([f'{x}s' for x in 字符串长度])
    吸1 = struct.unpack(文hint, b[4+n*4:])
    吸1 = [x.decode('utf8') for x in 吸1]
    return [*zip(吸0, 吸1)]


def _load2(b: bytes):
    assert b[:6] == b'yn0001', '版本不对'
    n = struct.unpack('i', b[6:10])[0]
    吸0 = struct.unpack(f'{n}e', b[10:10+n*2])
    吸1 = json.loads(b[10+n*2:])
    assert len(吸0) == len(吸1), '数据不完整'
    return [*zip(吸0, 吸1)]


def load(b: bytes):
    if b.startswith(b'yn0001'):
        return _load2(b)
    return _load1(b)

import sqlite3
import multiprocessing
import itertools
import typing 
from pathlib import Path

batch_size = 100000
processes_num = 6

base_path = "E:/tmp/internet_dataset"
domain_path = f"{base_path}/domain"
key_path = f"{base_path}/key"
page_path = f"{base_path}/page"

db_path = f"{base_path}/internet_dataset.sqlite"
dump_path = f"{base_path}/key_dump"

conn = sqlite3.connect(db_path)
cur = conn.cursor()

def batched(iterable, n):
      "Batch data into lists of length n. The last batch may be shorter."
      # batched('ABCDEFG', 3) --> ABC DEF G
      if n < 1:
          raise ValueError('n must be >= 1')
      it = iter(iterable)
      while (batch := list(itertools.islice(it, n))):
          yield batch
          
def flat(iterable):
    return list(itertools.chain(*iterable))

def create_table(tab: str):
    cur.execute(f'create table {tab} (data text)')

def insert_impl(p: Path) -> typing.Optional[list[tuple[str]]]:
    with open(p, 'rb') as f:
        try:
            raw = brotli.decompress(f.read())
        except Exception as e:
            print(f'decompress error: {p}: {e}')
            return None
        
        if str(p).replace('\\', '/').startswith(key_path.replace('\\', '/')):
            rel_path = p.parts
            keyword = ''.join(rel_path[rel_path.index('键') + 1:]).strip('_')
            
            data = (keyword, load(raw))
            try:
                return [(json.dumps(data, ensure_ascii=False),)]
            except Exception as e:
                print(f'json error[97]: {p}: {data}: {e}')
                return None
        else:
            try:
                return [(json.dumps(item, ensure_ascii=False),) for item in json.loads(raw)]
            except Exception as e:
                print(f'json error[87]: {p}: {e}')
                return None

def insert_table(path: str, tab: str):
    count = 0
    sql = f'insert into {tab} values (?)'
    
    with multiprocessing.Pool(processes=6) as thread_pool:
        for files in batched(filter(lambda p: p.is_file(), Path(path).rglob('*')), batch_size):
            data = [d for d in flat(filter(lambda x: x is not None, thread_pool.map(insert_impl, files)))]
            cur.executemany(sql, data)
            count += len(data)
            print(f'insert {count} rows')


def dump_impl(p: Path):
    with open(p, 'rb') as f:
        try:
            raw = brotli.decompress(f.read())
        except Exception as e:
            print(f'decompress error: {p}: {e}')
            return None
        
        rel_path = p.parts
        keyword = ''.join(rel_path[rel_path.index('键') + 1:]).strip('_')
            
        data = (keyword, load(raw))
        try:
            out = json.dumps(data, ensure_ascii=False)
            with open(f'{dump_path}/{keyword}.json', 'w', encoding='utf8') as f:
                f.write(out)
        except Exception as e:
            print(f'json error[97]: {p}: {data}: {e}')

def dump_key():
    with multiprocessing.Pool(processes=4) as thread_pool:
        thread_pool.map(dump_impl, filter(lambda p: p.is_file(), Path(key_path).rglob('*')))
    
    
if __name__ == '__main__':
    create_table('domain')
    insert_table(domain_path, 'domain')
    
    create_table('page')
    insert_table(page_path, 'page')

    create_table('key')
    insert_table(key_path, 'key')

    # print(load(brotli.decompress(open(r'E:\tmp\internet_dataset\key\键\10\0bienveillante_', 'rb').read())))

    dump_key()

    conn.commit()
    conn.close()