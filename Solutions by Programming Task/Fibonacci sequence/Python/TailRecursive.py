def Fib(x):
    f = lambda p, n, i: n if i == 0 else f(p + n, p, i - 1)
    return f(1, 0, x)
