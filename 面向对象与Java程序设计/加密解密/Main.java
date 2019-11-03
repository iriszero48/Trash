import java.util.Arrays;
import java.util.Scanner;
public class Main
{
    public static void main(String[] args)
    {
        System.out.println(Arrays.stream(new Scanner(System.in).nextLine().split("")).map(i -> Character.toString((char)(i.toCharArray()[0] + 3))).reduce((a, b) -> a + b).get());
        System.out.println(Arrays.stream(new Scanner(System.in).nextLine().split("")).map(i -> Character.toString((char)(i.toCharArray()[0] - 3))).reduce((a, b) -> a + b).get());
    }
}
