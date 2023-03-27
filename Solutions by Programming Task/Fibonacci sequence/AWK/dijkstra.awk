func fib_impl(a, b, p, q, i) {
    if (i == 0) return b
    return and(i, 1) == 0 ?
        fib_impl(a, b, p ^ 2 + q ^ 2, q ^ 2 + 2 * p * q, int(i / 2)) :
        fib_impl(b * q + a * q + a * p, b * p + a * q, p, q, i - 1)
}

func fib(x) {
    return fib_impl(1, 0, 0, 1, x)
}

BEGIN {
    for (i = 0; i <= 10; ++i) {
        print fib(i)
    }
}
