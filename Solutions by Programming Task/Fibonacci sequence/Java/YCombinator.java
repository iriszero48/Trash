import java.util.function.Function;

public class Main {
    interface RecFunc<F> extends Function<RecFunc<F>, F> { }
    private static <A,B> Function<A,B> Y(Function<Function<A,B>, Function<A,B>> f) {
        RecFunc<Function<A,B>> r = y -> f.apply(x -> y.apply(y).apply(x));
        return r.apply(r);
    }

    private static final Function<Integer, Integer> Fib = Y(f -> n -> n < 2 ? n : f.apply(n - 1) + f.apply(n - 2));

    public static void main(String[] args) {
        System.out.println(Fib.apply(10));
    }
}
