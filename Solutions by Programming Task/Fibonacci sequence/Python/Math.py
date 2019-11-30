from math import *

Fib = lambda x : int(2 ** -x * ((1 + sqrt(5)) ** x - (-1 + sqrt(5)) ** x * cos(pi * x)) / sqrt(5))
