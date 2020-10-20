#假设一段楼梯共 15 个台阶，小明一步最多能上 3 个台阶。编写程序计算小明上这段楼梯一共有多少种方法。要求给出递推法和递归法两种代码。
#递推法
p, n, n2 = 0, 1, 1
for i in range(16):
    p, n, n2 = n, n2, p + n + n2
print(p)
#递归法
def fib(x):
    if x < 2:
        return max(0, x)
    return fib(x - 1) + fib(x - 2) + fib(x - 3)


print(fib(16))
