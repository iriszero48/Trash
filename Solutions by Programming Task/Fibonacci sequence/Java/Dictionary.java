import java.math.BigInteger;
import java.util.Map;
import java.util.concurrent.ConcurrentSkipListMap;

public class Main {
    static final Map<BigInteger, BigInteger> dict = new ConcurrentSkipListMap<>(
            Map.of(BigInteger.ZERO, BigInteger.ZERO, BigInteger.ONE, BigInteger.ONE));

    private static BigInteger fib(BigInteger x) {
        return dict.computeIfAbsent(x, k -> fib(k.subtract(BigInteger.ONE)).add(fib(k.subtract(BigInteger.TWO))));
    }

    public static void main(String[] args) {
        System.out.println(fib(BigInteger.valueOf(10)));
    }
}
