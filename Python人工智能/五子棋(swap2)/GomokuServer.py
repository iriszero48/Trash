from socket import *
from threading import *
import json
import traceback


def judge(player_):
    for x_, y_ in player_:
        if (x_ - 1, y_) not in player_:
            why = {(x_, y_)}
            while (x_ + 1, y_) in player_:
                x_ += 1
                why.add((x_, y_))
                if len(why) == 5:
                    if (x_ + 1, y_) not in player_:
                        return why
                    else:
                        break
        if (x_, y_ - 1) not in player_:
            why = {(x_, y_)}
            while (x_, y_ + 1) in player_:
                y_ += 1
                why.add((x_, y_))
                if len(why) == 5:
                    if (x_, y_ + 1) not in player_:
                        return why
                    else:
                        break
        if (x_ - 1, y_ - 1) not in player_:
            why = {(x_, y_)}
            while (x_ + 1, y_ + 1) in player_:
                x_ += 1
                y_ += 1
                why.add((x_, y_))
                if len(why) == 5:
                    if (x_ + 1, y_ + 1) not in player_:
                        return why
                    else:
                        break
        if (x_ + 1, y_ - 1) not in player_:
            why = {(x_, y_)}
            while (x_ - 1, y_ + 1) in player_:
                x_ -= 1
                y_ += 1
                why.add((x_, y_))
                if len(why) == 5:
                    if (x_ - 1, y_ + 1) not in player_:
                        return why
                    else:
                        break
    return None


inited = False
white = set()
black = set()
count = 0
lock = Lock()
users = []
addrs = []
swap = False
swap2 = False
skt = socket(AF_INET, SOCK_DGRAM)
skt.bind((gethostname(), 27015))
while True:
    rawData, addr = skt.recvfrom(1024)
    lock.acquire()


    def resp(data_, addr_=addr):
        res = json.dumps({'data': data_})
        skt.sendto(res.encode('utf-8'), addr_)
        print(resp.__name__, addr_, res)


    try:
        jsonData = json.loads(rawData.decode('utf-8'))
        print(addr, jsonData)
        user, data = jsonData['data']
        if len(users) < 2:
            if data == 'join':
                users.append(user)
                addrs.append(addr)
                resp('black' if len(users) == 1 else 'white')
        elif user not in users:
            raise ValueError()
        elif swap:
            if data == 'white' or data == 'black':
                resp(data, addrs[1] if swap2 else addrs[0])
                if swap2:
                    if data == 'white':
                        users.reverse()
                        addrs.reverse()
                else:
                    if data == 'black':
                        users.reverse()
                        addrs.reverse()
                swap = False
                swap2 = False
                inited = True
            elif data == 'cont' and not swap2:
                resp(data, addrs[0])
                swap = False
                swap2 = True
            else:
                raise ValueError()
            count += 1
        else:
            data_ = data[0], data[1]
            if data_ not in black and data_ not in white:
                x, y = data_
                player = count & 1
                if (inited and user == users[(player + 1) % 2]) \
                        or (count < 3 and user != users[0]) \
                        or (not inited and 3 <= count < 5 and user != users[1]):
                    raise ValueError()
                else:
                    (white if player == 1 else black).add(data_)
                    flag = judge(white if player == 1 else black)
                    if count == 2 or (swap2 and count == 4):
                        swap = True
                    else:
                        count += 1
                    if flag is not None:
                        win = sorted(list(flag))
                        x0, y0 = win[0]
                        x1, y1 = win[-1]
                        for addr_ in addrs:
                            resp((user, x, y, x0, y0, x1, y1), addr_)
                        inited = False
                        white = set()
                        black = set()
                        count = 0
                        users = []
                        addrs = []
                        swap = False
                        swap2 = False
                    else:
                        if count < 3:
                            dest = addrs[1]
                        elif count < 5:
                            dest = addrs[0]
                        else:
                            dest = addrs[(player + 1) % 2]
                        resp(data, dest)

    except UnicodeDecodeError:
        pass
    except:
        traceback.print_exc()
    finally:
        lock.release()
