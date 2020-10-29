from tkinter import *
from math import *
from tkinter.messagebox import *
from threading import *

root = Tk()
white = set()
black = set()
count = 0
lock = Lock()
step = 30
board = Canvas(root, bg='white', height=step * 16, width=step * 16)
for i in range(15):
    board.create_line(step + i * step, step, step + i * step, step * 15)
for i in range(15):
    board.create_line(step, step + i * step, step * 15, step + i * step)


def judge(player_):
    for x_, y_ in player_:
        why = {(x_, y_)}
        while (x_ + 1, y_) in player_:
            x_ += 1
            why.add((x_, y_))
            if len(why) == 5:
                return why
        why = {(x_, y_)}
        while (x_, y_ + 1) in player_:
            y_ += 1
            why.add((x_, y_))
            if len(why) == 5:
                return why
        why = {(x_, y_)}
        while (x_ + 1, y_ + 1) in player_:
            x_ += 1
            y_ += 1
            why.add((x_, y_))
            if len(why) == 5:
                return why
        why = {(x_, y_)}
        while (x_ - 1, y_ + 1) in player_:
            x_ -= 1
            y_ += 1
            why.add((x_, y_))
            if len(why) == 5:
                return why
    return None


def board_click(ev):
    lock.acquire()
    global count
    clamp = lambda x: max(1, min(15, x))
    pos = clamp(floor(ev.x / step + 1 - 0.5)), clamp(floor(ev.y / step + 1 - 0.5))
    if pos not in black and pos not in white:
        x, y = pos
        if count & 1 == 1:
            white.add(pos)
            color = 'white'
            flag = judge(white)
        else:
            black.add(pos)
            color = 'black'
            flag = judge(black)
        board.create_oval(x * step - step / 2, y * step - step / 2, x * step + step / 2, y * step + step / 2,
                          fill=color)
        count += 1
        if flag is not None:
            board.unbind('<Button-1>')
            win = sorted(list(flag))
            x0, y0 = win[0]
            x1, y1 = win[-1]
            board.create_line(x0 * step, y0 * step, x1 * step, y1 * step, fill='red')
            showinfo(message=f'{color} win')
    lock.release()


board.bind('<Button-1>', board_click)
board.pack()
root.mainloop()
