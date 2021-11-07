fn fib(x: usize) -> usize {
    match x {
        | n if n < 2 => n,
        | _ => fib(x - 1) + fib(x - 2)
    }
}

fn main() {
    println!("{}", fib(10))
}
