use std::collections::HashMap;

fn fib_impl(dict: &mut HashMap<u64, u64>, x: u64) -> u64 {
    if let Some(v) = dict.get(&x) {
        return *v
    }

    let v = match x {
        n if n < 2 => n,
        _ => fib_impl(dict, x - 1) + fib_impl(dict, x - 2)
    };

    dict.insert(x, v);
    v
}

fn fib(x: u64) -> u64 {
    let mut dict: HashMap<u64, u64> = (0..1).map(|x:u64| {(x, x)}).collect();
    fib_impl(&mut dict, x)
}

fn main() {
    println!("{}", fib(10));
}
