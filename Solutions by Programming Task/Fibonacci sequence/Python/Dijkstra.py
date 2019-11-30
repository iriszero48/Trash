def Fib(x):
    def f(a, b, p, q, i):
        if i == 0:
            return b
        return f(a, b, p * p + q * q, q * q + 2 * p * q, i // 2) if i % 2 == 0 else f(b * q + a * q + a * p, b * p + a * q, p, q, i - 1)
    return f(1, 0, 0, 1, x)
