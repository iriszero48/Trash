#使用tkinter开发猜数字游戏，运行效果如下图所示。
#程序运行时，系统生成一个1024以内的随机数，然后提示用户进行猜测并根据用户输入进行必要的提示（猜对了、太大了、太小了），如果猜对则提前结束程序。程序要统计玩家猜的次数，如果次数用完仍没有猜对，提示游戏结束并给出正确答案。
from tkinter import *
import random

done = False
times = 1
target = random.SystemRandom().randint(1, 1024)
root = Tk()
left = Frame(root)
right = Frame(root)
right_cent = Frame(right)
user_input = Entry(right_cent)
output = Label(right, text='')
upValue = 1024
downValue = 1
up = Label(left, text=upValue)
down = Label(left, text=downValue)


def submit_func(*_):
    try:
        global times, done, downValue, upValue
        if done:
            return
        answer = int(user_input.get())
        if not (1 <= answer <= 1024 and answer % 1 == 0):
            raise ValueError()
        if answer == target:
            output.config(text="猜对了")
            done = True
        elif answer < target:
            output.config(text="太小了")
            if downValue < answer:
                downValue = answer
                down.config(text=answer)
        else:
            output.config(text="太大了")
            if upValue > answer:
                upValue = answer
                up.config(text=answer)
        times += 1
        if times > 10:
            output.config(text="游戏结束，答案是{}".format(target))
            done = True
    except:
        output.config(text="无效值")


user_input.bind('<Return>', submit_func)
up.pack(side='top')
Label(left, text='').pack(side='top')
output.pack(side='top')
user_input.pack(side='left')
Button(right_cent, text='猜', command=submit_func).pack(side='left')
down.pack(side='bottom')
Button(right, text='关闭', command=lambda: root.destroy()).pack(side='bottom')
right_cent.pack(side='top')
left.pack(side='left')
right.pack(side='left')
root.title('猜数字游戏')
root.mainloop()
