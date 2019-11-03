import java.util.Arrays;
import java.util.stream.IntStream;

public class Main
{
    private static void NarcissisticNumbers()
    {
        IntStream.range(100, 1000).filter(i -> Arrays.stream((i + "").split("")).mapToDouble(j -> Math.pow(Integer.parseInt(j), 3)).sum() == i).forEach(System.out::println);
    }
    public static void main(String[] args)
    {
        NarcissisticNumbers();
    }
}
