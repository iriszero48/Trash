import collections
import random

EllipticCurve = collections.namedtuple('EllipticCurve', 'name p a b g n h')

curve = EllipticCurve(
    'secp256k1',
    p=int(input('p=')),
    a=int(input('a=')),
    b=int(input('b=')),
    g=(int(input('g.x=')),
       int(input('g.y='))),
    n=int(input('k=')),
    h=1,
)

def inverse_mod(k, p):
    if k == 0:
        raise ZeroDivisionError('division by zero')
    if k < 0:
        return p - inverse_mod(-k, p)
    s, old_s = 0, 1
    t, old_t = 1, 0
    r, old_r = p, k
    while r != 0:
        quotient = old_r // r
        old_r, r = r, old_r - quotient * r
        old_s, s = s, old_s - quotient * s
        old_t, t = t, old_t - quotient * t
    gcd, x, y = old_r, old_s, old_t
    assert gcd == 1
    assert (k * x) % p == 1
    return x % p

def is_on_curve(point):
    if point is None:
        return True
    x, y = point
    return (y * y - x * x * x - curve.a * x - curve.b) % curve.p == 0

def point_neg(point):
    assert is_on_curve(point)
    if point is None:
        return None
    x, y = point
    result = (x, -y % curve.p)
    assert is_on_curve(result)
    return result

def point_add(point1, point2):
    assert is_on_curve(point1)
    assert is_on_curve(point2)
    if point1 is None:
        return point2
    if point2 is None:
        return point1
    x1, y1 = point1
    x2, y2 = point2
    if x1 == x2 and y1 != y2:
        return None
    if x1 == x2:
        m = (3 * x1 * x1 + curve.a) * inverse_mod(2 * y1, curve.p)
    else:
        m = (y1 - y2) * inverse_mod(x1 - x2, curve.p)
    x3 = m * m - x1 - x2
    y3 = y1 + m * (x3 - x1)
    result = (x3 % curve.p,
              -y3 % curve.p)
    assert is_on_curve(result)
    return result

def scalar_mult(k, point):
    assert is_on_curve(point)
    if k < 0:
        return scalar_mult(-k, point_neg(point))
    result = None
    addend = point
    while k:
        if k & 1:
            result = point_add(result, addend)
        addend = point_add(addend, addend)
        k >>= 1
    assert is_on_curve(result)
    return result

def make_keypair():
    private_key = curve.n
    public_key = scalar_mult(private_key, curve.g)
    return private_key, public_key

print('Curve:', curve.name)
private_key, public_key = make_keypair()
print("private key:", hex(private_key))
print("public key: (0x{:x}, 0x{:x})".format(*public_key))
