import csv
import re

if __name__ == '__main__':
    """fs = open('...', 'r', newline='')
    total = 0
    for row in csv.reader(fs):
        print(row)
        total += int(row[0])
    print(total)"""

    d = """ """

    dd = [i.strip() for i in d.split('\n') if i != '']

    count = 0
    size = 0

    for i in range(len(dd) // 2):
        ddd = [i for i in dd[i * 2:i * 2 + 2]]


        def get(x: str):
            c = int(x.split(':')[1].split(',')[0].strip())
            s = int(x.split(':')[-1].strip())
            return c, s


        a, b = map(get, ddd)
        count += b[0] - a[0]
        size += b[1] - a[1]

    print(count, size)
