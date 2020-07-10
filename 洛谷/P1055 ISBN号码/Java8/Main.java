import java.util.Arrays;
import java.util.Scanner;
import java.util.stream.IntStream;

public class Main
{
    public static void main(String[] args)
    {
        String cin = new Scanner(System.in).nextLine();
        int[] data = Arrays.stream(cin.replace("-", "").split("")).limit(9).mapToInt(Integer::parseInt).toArray();
        String res = (IntStream.range(0, 9).map(x -> data[x] * (x + 1)).sum() % 11 + "").replace("10", "X");
        System.out.println(cin.substring(12).equals(res) ? "Right" : cin.substring(0, 12) + res);
    }
}
