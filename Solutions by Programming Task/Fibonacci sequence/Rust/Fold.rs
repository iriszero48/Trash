fn fib(x: i32) -> i32 {
    (0..x).fold([0, 1], |s, _| [s[1], s.iter().sum()])[0]
}

fn main() {
    println!("{}", fib(10));
}
