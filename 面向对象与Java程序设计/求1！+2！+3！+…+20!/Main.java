import java.util.function.Function;

class Main 
{
    interface Rec<F> extends Function<Rec<F>, F> { }
    private static <A,B> Function<A,B> Y(Function<Function<A,B>, Function<A,B>> f) 
    {
        Rec<Function<A,B>> r = w -> f.apply(x -> w.apply(w).apply(x));
        return r.apply(r);
    }

    public static Long FactorialSum(Long input)
    {
        Function<Long, Long> fac = Y(f -> n -> n > 1 ? n * f.apply(n - 1) : 1);
        Function<Long,Long> facSum = Y(f -> n -> n > 0 ? fac.apply(n) + f.apply(n - 1) : 0);
        return facSum.apply(input);
    }

    public static void main(String[] args) 
    {
        System.out.println(FactorialSum((long)20));
    }
}
