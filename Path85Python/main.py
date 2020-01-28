import os, sys
from itertools import islice, repeat, chain

table = r"!#$%&'()+,-.0123456789;=@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz{}~"

def ChunkBySize(lst, size):
    lst = iter(lst)
    return list(iter(lambda: list(islice(lst, size)), []))

def PadRight(lst, padding, size):
    return list(islice(chain(lst, repeat(padding)), size))

def Encode(data):
    block = ChunkBySize(data, 4)
    lastLen = len(block[-1])
    block[-1] = PadRight(block[-1], 0, 4)
    return "".join([table[y] for x in map(lambda x: [x // 52200625, x // 614125 % 85, x // 7225 % 85, x // 85 % 85, x % 85], map(lambda x: x[0] << 24 | x[1] << 16 | x[2] << 8 | x[3], block)) for y in x][:None if lastLen == 4 else lastLen-4])

def Decode(data):
    dataLen = len(data)
    paddingLen = dataLen if dataLen % 5 == 0 else dataLen // 5 + 5
    return bytes([y & 0xff for x in map(lambda x: sum([x[0] * 52200625, x[1] * 614125, x[2] * 7225, x[3] * 85, x[4]]), ChunkBySize(map(lambda x: table.index(x), data.ljust(paddingLen,'~')),5)) for y in [x >> 24, x >> 16, x >> 8, x]][:None if dataLen == paddingLen else dataLen - paddingLen])

