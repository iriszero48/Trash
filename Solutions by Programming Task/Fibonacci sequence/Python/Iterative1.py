def Fib(x):
    d = [0, 1]
    for i in range(x):
        d = [d[1], sum(d)]
    return d[0]
