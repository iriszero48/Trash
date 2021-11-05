public class Main {
    private static Long Fib(Long x) {
        record Impl() {
            public static Long Get(Long a, Long b, Long p, Long q, Long i) {
                return i == 0L
                        ? b
                        : (i % 2L == 0L
                            ? Get(a, b, p * p + q * q, q * q + 2L * p * q, i / 2L)
                            : Get(b * q + a * q + a * p, b * p + a * q, p, q, i - 1L));
            }
        }
        return Impl.Get(1L, 0L, 0L, 1L, x);
    }

    public static void main(String[] args) {
        System.out.println(Fib(10L));
    }
}
