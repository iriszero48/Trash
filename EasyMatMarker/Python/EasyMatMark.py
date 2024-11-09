import time
import random

# a = [float(i + 1) / float(128 * 128) for i in range(128 * 128)]
a = [random.random() for i in range(128 * 128)]
a[127 * 128 + 127] = 1.0
b = [1 for i in range(128 * 128)]
c = [0 for i in range(128 * 128)]
sum_value = 0


def add(result, x):
    for i in range(128 * 128):
        result[i] += x


def div(result, x):
    for i in range(128 * 128):
        result[i] /= x


def mul(a_, b_, result):
    for row in range(128):
        for col in range(128):
            v = 0
            for i in range(128):
                v += a_[row * 128 + i] * b_[i * 128 + col]
            result[row * 128 + col] = v


tp1 = time.time()

for i in range(1000):
    add(b, i)
    mul(a, b, c)

    add(b, i + 1)
    mul(c, b, a)

    div(a, c[127 * 128 + 127])

sum_value = sum(a)

tp2 = time.time()

print((tp2 - tp1) * 1000., sum_value)
