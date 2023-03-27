func fib_impl(x, f1, f2) {
    if (x in dict) {
        return dict[x]
    }
    f1 = fib(int(x / 2) + 1)
    f2 = fib(int((x - 1) / 2))
    dict[x] = and(x, 1) == 1 ? f1 * f1 + f2 * f2 : f1 * f1 - f2 * f2
    return dict[x]
}

func fib(x) {
    return fib_impl(x)
}

BEGIN {
    dict[0] = 0
    dict[1] = 1
    dict[2] = 1

    for (i = 0; i <= 10; ++i) {
        print fib(i)
    }
}
