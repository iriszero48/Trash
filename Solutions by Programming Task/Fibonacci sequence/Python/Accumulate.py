from itertools import *

Fib = lambda x: list(accumulate(chain([[0, 1]], range(x)),lambda s, x: [s[1],sum(s)]))[-1][0]
