func fib(x) {
    return x < 2 ? x : fib(x - 1) + fib(x - 2)
}

BEGIN {
    for (i = 0; i <= 10; ++i) {
        print fib(i)
    }
}
