import requests as req


def get_n(n):
    if n == 0:
        return '\n'.join([
            '向右 1 步',
            '拿起 1 个物品',
            '向左 1 步',
            '放下 1 个物品'
        ])
    if n == 1:
        return '\n'.join([
            '向右 1 步',
            '向右 1 步',
            '拿起 1 个物品',
            '向左 1 步',
            '向左 1 步',
            '放下 1 个物品'
        ])
    return '\n'.join([
        '向右 1 步',
        f'拿起 {n} 个物品',
        '向右 1 步',
        '放下 1 个物品',
        '如果手上的物品大于等于 1 向上跳转 2 行',
        '放下 1 个物品',
        f'拿起 {n + 1} 个物品',
        '向左 1 步',
        '放下 1 个物品',
        '如果手上的物品大于等于 1 向上跳转 2 行'
    ])
from hashlib import sha256

def nhash(msg):
    return sha256(msg.strip().encode('utf-8')).hexdigest()

progs = []

def fuck(prog, seed):
    return req.post(
        "http://202.38.93.111:12077/verify",
        json={
            "token": "1486:MEYCIQDJfe4xEKDPPqE3i+qiQfV7ElBon20Cn5xpNT9gtyhAAwIhAOvDB4zdM2e9mmjhD8Qk/Nh74KfnEP72H43dz0bgOfA+",
            "level": 2,
            "seed": seed,
            "programs": prog,
            "executions": [0]
        },
        headers={
            'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:90.0) Gecko/20100101 Firefox/90.0',
            'Referer': 'http://202.38.93.111:12077/',
            'Origin': 'http://202.38.93.111:12077',
            'Content-Type': 'application/json'
        }
    )


if __name__ == '__main__':
    # dt = [5,10,11,4,26,26]
    # dt = [5,22,24,6,26,5]
    #dt = list(map(int, input().strip().split(',')))
    # print(('\n'.join(map(get_n, dt)) + '\n放下盘子').replace('\n',r'\n'))
    #print(('\n'.join(map(get_n, dt)) + '\n放下盘子'))
    # print('\n'.join(map(get_n, dt)))

    #while True:
    #    dt = list(map(int, input().strip().split(',')))
    #    print(('\n'.join(map(get_n, dt)) + '\n放下盘子'))

    #exit(0)
    dt = [5,22,24,6,26,5]
    prog = '\n'.join(map(get_n, dt)) + '\n放下盘子'
    progs.append(prog)
    seed = ''
    #seed = '07b778bc71385b2d52f957395a0c0625dacb66c2f595245a742b71004225adf7'
    while True:
        ret = fuck([prog], seed)
        print(ret.text)
        if 'flag' in ret.text:
            print(ret)
            exit(0)
        ret_json = ret.json()
        st = ret_json["status"]
        msg = ret_json["message"]
        if st == "error" and msg.startswith("Seed"):
            seed = msg.split(' ')[-1].replace('.', '')
        if st == "error" and msg.startswith("No win"):
            dt = list(map(int, msg.split(' ')[-1].replace('.', '').replace('[', '').replace(']', '').split(',')))
            prog = '\n'.join(map(get_n, dt)) + '\n放下盘子'
            progs.append(prog)
            time.sleep(1)

        #break
