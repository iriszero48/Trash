from functools import *

Fib = lambda x: reduce(lambda s, x: [s[1],sum(s)], range(x), (0, 1))[0]
