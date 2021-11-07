use std::collections::HashMap;

fn fib_impl(dict: &mut HashMap<usize, usize>, x: usize) -> usize {
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

fn fib(x: usize) -> usize {
    let mut dict: HashMap<_, _> = (0..1).map(|x:usize| {(x, x)}).collect();
    fib_impl(&mut dict, x)
}

fn main() {
    println!("{}", fib(10));
}
