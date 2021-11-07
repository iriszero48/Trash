fn fib(x: isize) -> isize {
    let mut pow_two;
    if (x & -x) == x {
        pow_two = x;
    } else {
        pow_two = x - 1;
		pow_two |= pow_two >> 1;
		pow_two |= pow_two >> 2;
		pow_two |= pow_two >> 4;
		pow_two |= pow_two >> 8;
		pow_two |= pow_two >> 16;
		pow_two += 1;
		pow_two /= 2
    }
    let mut d = (1, 1, 1, 0); // i q r s
    while d.0 < pow_two {
        d = (d.0 * 2, d.1 * d.1 + d.2 * d.2, d.2 * (d.1 + d.3), d.2 * d.2 + d.3 * d.3)
    }
    while d.0 < x {
        d = (d.0 + 1, d.1 + d.2, d.1, d.2)
    }
    d.2
}

fn main() {
    println!("{}", fib(10))
}
