func fib_impl_pow2(x) {
    x -= 1
    x = or(x, rshift(x, 1))
    x = or(x, rshift(x, 2))
    x = or(x, rshift(x, 4))
    x = or(x, rshift(x, 8))
    x = or(x, rshift(x, 16))
    x += 1
    return x / 2
}

func fib_impl_loop1(pow2, i, q, r, s) {
    return i < pow2 ?
        fib_impl_loop1(pow2, i * 2, q * q + r * r, r * (q + s), r * r + s * s) :
        i " " q " " r " " s
}

func fib_impl_loop2(x, i, q, r, s) {
    return i < x ?
        fib_impl_loop2(x, i + 1, q + r, q, r) :
        r
}

func fib_impl(x, v) {
    if (x == 0) return x

    split(fib_impl_loop1(fib_impl_pow2(x), 1, 1, 1, 0), v, " ")
    return fib_impl_loop2(x, v[1], v[2], v[3], v[4])
}

func fib(x) {
    return fib_impl(x)
}

BEGIN {
    for (i = 0; i <= 10; ++i) {
        print fib(i)
    }
}
