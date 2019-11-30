def Fib(x):
    p = 0
    n = 1
    for i in range(x):
        p, n = n, p + n
    return p
