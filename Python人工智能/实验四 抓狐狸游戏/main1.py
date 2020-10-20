#编写程序，模拟抓狐狸小游戏。假设一共有一排 5 个洞口，小狐狸最开始的时候在其中一个洞口，然后玩家随机打开一个洞口，如果里面有狐狸就抓到了。如果洞口里没有狐狸就第二天再来抓，但是第二天狐狸会在玩家来抓之前跳到隔壁洞口里。
import random
target = random.randint(0, 4)
while True:
    try:
        x = int(input('在哪(1-5):'))
        if not 1 <= x <= 5:
            raise ValueError()
        if x == target + 1:
            print('抓住')
            break
        else:
            print('没抓住')
            target = (5 + target + -1 ** random.randint(1, 2)) % 5
    except:
        print('无效值')
