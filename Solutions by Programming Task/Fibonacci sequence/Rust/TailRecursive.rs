fn fib_impl(p: usize, n: usize, i: usize) -> usize {
    match i {
        0 => p,
        _ => fib_impl(n, p + n, i - 1)
    }
}

fn fib(x: usize) -> usize {
    fib_impl(0, 1, x)
}

fn main() {
    println!("{}", fib(10))
}
