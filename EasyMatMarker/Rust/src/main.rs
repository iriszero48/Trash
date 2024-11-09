use rand::Rng;

const SIZE: usize = 128 * 128;

fn div(mat: &mut [f64; SIZE], x: f64) {
    for i in 0..SIZE {
        mat[i] /= x;
    }
}

fn add(mat: &mut [f64; SIZE], x: f64) {
    for i in 0..SIZE {
        mat[i] += x;
    }
}

fn mul(a: [f64; SIZE], b: [f64; SIZE], c: &mut [f64; SIZE]) {
    for row in 0..128 {
        for col in 0..128 {
            let mut v: f64 = 0.;
            for i in 0..128 {
                v += a[row * 128 + i] * b[i * 128 + col];
            }
            c[row * 128 + col] = v;
        }
    }
}

fn main() {
    let mut a: [f64; SIZE] = std::array::from_fn(|_| rand::thread_rng().gen_range(0.0 .. 1.0) );
    a[128 * 128 - 1] = 1.0;
    let mut b: [f64; SIZE] = [1.; SIZE];
    let mut c: [f64; SIZE] = [0.; SIZE];
    let mut sum_value: f64 = 0.;

    let tp1 = std::time::Instant::now();

    for i in 0..1000 {
        add(&mut b, i as f64);
        mul(a, b, &mut c);

        add(&mut b, i as f64 + 1.);
        mul(c, b, &mut a);

        div(&mut a, c[127 * 128 + 127]);
    }

    for x in a {
        sum_value += x;
    }

    let tp2 = tp1.elapsed();

    std::println!("{} {}", tp2.as_millis(), sum_value);
}
