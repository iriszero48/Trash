from binascii import unhexlify
from operator import mul


def NumToBytes(n):
    return unhexlify(hex(n)[2:])


def FuckRsaFactored(primes,n,c,e):
    d = inverse_mod(e, reduce(mul, [i-1 for i in primes]))
    m = pow(c,d,n)
    return m
