from math import *
from tkinter.messagebox import *
from tkinter.simpledialog import *
from threading import *
from socket import *
import json

game_id = 'Gomoku(swap2)'
inited = False
root = Tk()
is_black = False
black_white = set()
count = 0
swap2 = False
lock = Lock()
skt = socket(AF_INET, SOCK_DGRAM)
user_id = askstring(game_id, 'plz input your id')
addr = askstring(game_id, 'plz input server\'s address')


def req(data):
    res = json.dumps({'data': (user_id, data)})
    skt.sendto(res.encode('utf-8'), (addr, 27015))
    print(req.__name__, res)


def recv():
    res = json.loads(skt.recvfrom(1024)[0].decode('utf-8'))
    print(recv.__name__, res)
    return res['data']


step = 30
board = Canvas(root, bg='white', height=step * 16, width=step * 16)
for i in range(15):
    board.create_line(step + i * step, step, step + i * step, step * 15)
for i in range(15):
    board.create_line(step, step + i * step, step * 15, step + i * step)


def update_title():
    root.title(f'{game_id} - {addr} - {user_id} - {"black" if is_black else "white"}')


def draw_call(x, y):
    global count
    lock.acquire()
    player = count & 1
    board.create_oval(x * step - step / 2, y * step - step / 2,
                      x * step + step / 2, y * step + step / 2,
                      fill='white' if player == 1 else 'black')
    count += 1
    lock.release()


def init_swap2():
    global is_black, swap2, inited
    resp = recv()
    if resp == 'cont':
        swap2 = True
        for _ in range(2):
            x, y = recv()
            draw_call(x, y)
        res = ask_swap2()
        req(res)
        swap2 = False
        inited = True
        if res == 'white':
            is_black = False
            update_title()
        else:
            is_black = True
            update_title()
            recv_next()
    elif resp == 'white':
        is_black = True
        update_title()
        inited = True
        recv_next()
    else:
        inited = True
        is_black = False
        update_title()


def swap2_comp():
    global is_black, inited
    resp = recv()
    update_title()
    if resp == 'black':
        is_black = False
        inited = True
        update_title()
    else:
        is_black = True
        inited = True
        update_title()
        recv_next()


def recv_next():
    re_ = recv()
    if len(re_) == 7:
        user, x, y, x0, y0, x1, y1 = re_
        board.unbind('<Button-1>')
        if user != user_id:
            draw_call(x, y)
        board.create_line(x0 * step, y0 * step, x1 * step, y1 * step, fill='red')
        showinfo(message=f'{user} win')
    else:
        x, y = re_
        draw_call(x, y)


def board_click(ev):
    # lock.acquire()
    global count, is_black, swap2
    clamp = lambda x: max(1, min(15, x))
    pos = clamp(floor(ev.x / step + 1 - 0.5)), clamp(floor(ev.y / step + 1 - 0.5))
    if pos not in black_white:
        player = count & 1
        if (is_black and count < 3) or (swap2 and not is_black and 3 <= count < 5) or (
                inited and is_black and player == 0) or (
                inited and not is_black and player == 1):
            x, y = pos
            black_white.add(pos)
            req(pos)
            if count == 2:
                if is_black:
                    Thread(target=init_swap2).start()
            elif swap2 and count == 4:
                if not is_black:
                    Thread(target=swap2_comp).start()
            draw_call(x, y)
            if inited:
                Thread(target=recv_next).start()
    # lock.release()


def ask_swap():
    return {True: 'black', False: 'white', None: 'cont'}[
        askyesnocancel('swap', 'yes to black, no to white, cancel to swap2')]


def ask_swap2():
    return {True: 'black', False: 'white'}[
        askyesno('swap', 'yes to black, no to white')]


board.bind('<Button-1>', board_click)
board.pack()


def init():
    global is_black, swap2, inited
    req('join')
    res = recv()
    print(res)
    if res == 'black':
        is_black = True
        update_title()
    else:
        for _ in range(3):
            fk = recv()
            x, y = fk
            draw_call(x, y)
        res = ask_swap()
        req(res)
        if res == 'black':
            inited = True
            is_black = True
            update_title()
            recv_next()
        elif res == 'cont':
            swap2 = True
        else:
            inited = True
            is_black = False
            update_title()


update_title()
Thread(target=init).start()
root.mainloop()
