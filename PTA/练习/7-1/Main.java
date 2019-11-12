import java.util.Arrays;
import java.util.Collections;
import java.util.Scanner;
import java.util.stream.IntStream;

public class Main
{
    public static void main(String[] args)
    {
        IntStream.rangeClosed(1,Integer.parseInt("1" + String.join("", Collections.nCopies(new Scanner(System.in).nextInt(), "0")))).filter(n -> Arrays.stream(String.valueOf(n).split("")).mapToInt(i -> (int)(Math.pow(Integer.parseInt(String.valueOf(i)),5))).sum() == n).forEach(System.out::println);
    }
}
