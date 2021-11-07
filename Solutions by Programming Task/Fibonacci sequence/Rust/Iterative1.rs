fn fib(x: usize) -> usize {
    std::iter::successors(Some((0,1)), |(p, n)| Some((*n, *p + *n))).skip(x).next().unwrap().0
}

fn main() {
    println!("{}", fib(10));
}
