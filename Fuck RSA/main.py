def egcd(a, b):
    if a == 0:
        return (b, 0, 1)
    else:
        g, y, x = egcd(b % a, a)
        return (g, x - (b // a) * y, y)


def inverse_mod(a, m):
    g, x, y = egcd(a, m)
    if g != 1:
        raise Exception('modular inverse does not exist')
    else:
        return x % m


def NumToBytes(n):
    return unhexlify(hex(n)[2:])


def FuckRsaFactored(primes,n,c,e):
    d = inverse_mod(e, reduce(mul, [i-1 for i in primes]))
    m = pow(c,d,n)
    return m
