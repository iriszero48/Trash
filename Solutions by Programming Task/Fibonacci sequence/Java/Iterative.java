import java.util.stream.IntStream;

public class Main {
    private static int Fib(int x) {
        var p = 0;
        var n = 1;
        for (var __ : IntStream.range(0, x).toArray()) {
            var sum = p + n;
            p = n;
            n = sum;
        }
        return p;
    }

    public static void main(String[] args) {
        System.out.println(Fib(10));
    }
}
