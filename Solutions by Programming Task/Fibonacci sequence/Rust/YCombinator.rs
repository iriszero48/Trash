trait RecFunc<R, T> { fn apply(&self, func: &dyn RecFunc<R, T>, param: T) -> R; }

impl <R, T, F> RecFunc<R, T> for F where F: Fn(&dyn RecFunc<R, T>, T) -> R {
    fn apply(&self, func: &dyn RecFunc<R, T>, param: T) -> R { self(func, param) }
}

fn y<R, T>(func: impl Fn(&dyn Fn(T) -> R, T) -> R) -> impl Fn(T) -> R {
    move |param| (& |x: &dyn RecFunc<R, T>, param| x.apply(x, param))(
         & |x: &dyn RecFunc<R, T>, a| func(& |b| x.apply(x, b), a), param)
}

fn fib(x: usize) -> usize {
    y(& |f: &dyn Fn(usize) -> usize, n| match n {
        n if n < 2 => n,
        _ => f(n - 1) + f(n - 2)
    })(x)
}

fn main() {
    println!("{}", fib(10));
}
