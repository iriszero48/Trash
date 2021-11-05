import java.util.Arrays;
import java.util.stream.IntStream;

public class Main {
    private static int Fib(int x) {
        var data = new int[]{0, 1};
        IntStream.range(0, x).forEach(__ -> {
            var sum = Arrays.stream(data).sum();
            data[0] = data[1];
            data[1] = sum;
        });
        return data[0];
    }

    public static void main(String[] args) {
        System.out.println(Fib(10));
    }
}
