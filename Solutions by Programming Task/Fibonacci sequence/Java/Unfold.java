import java.util.function.IntUnaryOperator;
import java.util.stream.IntStream;

public class Main {
    public static IntStream FibStream() {
        return IntStream.iterate(1, new IntUnaryOperator() {
            private int p = 0;
            @Override
            public int applyAsInt(int operand) {
                var sum = p + operand;
                p = operand;
                return sum;
            }
        });
    }
    public static long fib(long n) {
        return FibStream().limit(n).reduce((a, b) -> b).getAsInt();
    }

    public static void main(String[] args) {
        System.out.println(fib(10));
    }
}
