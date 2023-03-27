func fib_impl(x, p, n, v, i) {
    p = 0
    n = 1
    for (i = 0; i < x; ++i) {
        v = p + n
        p = n
        n = v
    }
    return p
}

func fib(x) {
    return fib_impl(x)
}

BEGIN {
    for (i = 0; i <= 10; ++i) {
        print fib(i)
    }
}
