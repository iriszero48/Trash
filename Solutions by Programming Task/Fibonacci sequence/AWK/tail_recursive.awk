func fib_impl(p, n, i) {
    return i == 0 ?
        p :
        fib_impl(n, p + n, i - 1)
}

func fib(x) {
    return fib_impl(0, 1, x)
}

BEGIN {
    for (i = 0; i <= 10; ++i) {
        print fib(i)
    }
}
