import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.stream.IntStream;
import java.util.concurrent.ThreadLocalRandom;

public class Main {
    public static final int ROW = 128;
    public static final int COL = 128;
    public static final int SIZE = ROW * COL;

    public static void add(double[] mat, double val) {
        for (var i = 0; i < SIZE; i++) {
            mat[i] += val;
        }
    }

    public static void div(double[] mat, double val) {
        for (var i = 0; i < SIZE; i++) {
            mat[i] /= val;
        }
    }

    public static void mul(double[] a, double[] b, double[] c) {
        for (var row = 0; row < ROW; row++) {
            for (var col = 0; col < COL; col++) {
                double v = 0;
                for (var i = 0; i < COL; i++) {
                    v += a[row * COL + i] * b[i * COL + col];
                }
                c[row * COL + col] = v;
            }
        }
    }

    public static void main(String[] args) {
        // var a = IntStream.range(0, SIZE).mapToDouble(x -> (x + 1.) / SIZE).toArray();
        var a = IntStream.range(0, SIZE).mapToDouble(_ -> ThreadLocalRandom.current().nextDouble()).toArray();
        a[128 * 128 - 1] = 1.0;
        var b = IntStream.range(0, SIZE).mapToDouble(_ -> 1.).toArray();
        var c = new double[SIZE];
        var sum_val = 0.;

        var tp1 = Instant.now();

        for (var i = 0; i < 1000; i++) {
            add(b, i);
            mul(a, b, c);

            add(b, i + 1);
            mul(c, b, a);

            div(a, c[127 * 128 + 127]);
        }

        for (var i = 0; i < SIZE; i++) {
            sum_val += a[i];
        }

        var tp2 = Instant.now();

        System.out.printf("%f %f%n", ChronoUnit.NANOS.between(tp1, tp2) / 1000. / 1000., sum_val);
    }
}