D = {0: 0, 1: 1, 2: 1}
def Fib(x):
    if x in D:
        return D[x]
    f1 = Fib(x // 2 + 1)
    f2 = Fib((x - 1) // 2)
    D[x] = (f1 * f1 + f2 * f2 if x & 1 else f1 * f1 - f2 * f2)
    return D[x]
