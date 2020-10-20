#假设一段楼梯共 15 个台阶，小明一步最多能上 3 个台阶。编写程序计算小明上这段楼梯一共有多少种方法。要求给出递推法和递归法两种代码。
#递推法
__import__('functools').reduce(lambda s, _: s[1:] + [sum(s)], range(16), [0, 1, 1])[0]
#递归法
(lambda f: (lambda x: x(x))(lambda y: f(lambda a: y(y)(a))))(lambda f: lambda n: max(0, n) if n < 2 else sum(map(f, range(n - 3, n))))(16)
