func fib(x) {
    return int(2 ^ -x * ((1 + sqrt(5)) ^ x - (-1 + sqrt(5)) ^ x * cos(atan2(0, -1) * x)) / sqrt(5))
}

BEGIN {
    for (i = 0; i <= 10; ++i) {
        print fib(i)
    }
}
