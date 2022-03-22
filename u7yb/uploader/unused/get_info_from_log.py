import csv

if __name__ == '__main__':
    db = {}
    fs = open('...', 'r', newline='')
    for row in csv.reader(fs):
        db[int(row[0])] = int(row[1])

    total_size = 0
    total_num = 0
    with open('...', 'r') as f:
        for log in f.readlines():
            item_id = int(log.split('[')[-1].split(']')[0])
            total_size += db[item_id]
            total_num += 1

    with open('...', 'r') as f:
        for log in f.readlines():
            item_id = int(log.split('[')[-1].split(']')[0])
            total_size += db[item_id]
            total_num += 1

    with open('...', 'r') as f:
        for log in f.readlines():
            item_id = int(log.split('[')[-1].split(']')[0])
            total_size += db[item_id]
            total_num += 1

    print(f'count: {total_num}')
    print(f'size: {total_size}')

    print("---------")
    print(f"db/count: {len(db)}")
    print(f"db/size: {sum([v for k,v in db.items()])}")
