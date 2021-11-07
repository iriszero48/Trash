fn fib(x: usize) -> usize {
    std::iter::successors(Some((0,1)), |(p, n)| Some((*n, *p + *n))).map(|(p, _)| p).skip(x).next().unwrap()
}

fn main() {
    println!("{}", fib(10));
}
