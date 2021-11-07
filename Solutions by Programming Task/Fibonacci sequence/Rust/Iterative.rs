fn fib(x: usize) -> usize {
    let mut d: (usize, usize) = (0, 1);
    for _ in 0..x {
        d = (d.1, d.0 + d.1)
    }
    d.0
}

fn main() {
    println!("{}", fib(10));
}
