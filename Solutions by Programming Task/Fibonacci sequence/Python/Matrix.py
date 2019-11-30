def Fib(x):
    if x == 0:
        return 0
    def prevPowTwo(x):
        if ((x & -x) == x):
            return x
        else:
            x -= 1
            x |= x >> 1
            x |= x >> 2
            x |= x >> 4
            x |= x >> 8
            x |= x >> 16
            x += 1
            return x / 2
    powTwo = prevPowTwo(x)
    q = r = i = 1
    s = 0
    while(i < powTwo):
        i, q, r, s = i * 2, q * q + r * r, r * (q + s), r * r + s * s
    while(i < x):
        i, q, r, s = i + 1, q + r, q, r
    return r
