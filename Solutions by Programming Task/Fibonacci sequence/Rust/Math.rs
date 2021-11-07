fn fib(x: usize) -> usize {
    (2_f64.powf(-(x as f64))
        * ((1_f64 + 5_f64.sqrt()).powf(x as f64)
            - (-1_f64 + 5_f64.sqrt()).powf(x as f64)
            * (std::f64::consts::PI * (x as f64)).cos())
        / 5_f64.sqrt()) as usize
}

fn main() {
    println!("{}", fib(10))
}
