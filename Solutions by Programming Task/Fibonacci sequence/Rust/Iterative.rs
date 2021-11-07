fn fib(x: usize) -> usize {
    let mut d: [usize; 2] = [0, 1];
    for _ in 0..x {
        d = [d[1], d.iter().sum()]
    }
    d[0]
}

fn main() {
    println!("{}", fib(10));
}
