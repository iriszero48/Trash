import java.util.Arrays;
import java.util.stream.Stream;

public class Main {
    private static int fib(int x) {
        return Stream.of(new int[x][]).reduce(new int[]{0, 1}, (a, b) -> new int[]{a[1], Arrays.stream(a).sum()})[0];
    }

    public static void main(String[] args) {
        System.out.println(fib(10));
    }
}
