import java.math.BigInteger;

public class Main {
    private static BigInteger fib(BigInteger x) {
        return x.compareTo(BigInteger.valueOf(2)) < 0
                ? x
                : fib(x.subtract(BigInteger.valueOf(1))).add(fib(x.subtract(BigInteger.valueOf(2))));
    }

    public static void main(String[] args) {
        System.out.println(fib(BigInteger.valueOf(10)));
    }
}
