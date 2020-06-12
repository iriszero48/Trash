import java.util.Arrays;
import java.util.Scanner;

public class Main
{
    public static void main(String... arguments)
    {
        Scanner scanner = new Scanner(System.in);
        int cin = scanner.nextInt();
        boolean x = (cin & 1) == 0;
        boolean y = cin > 4 && cin <= 12;
        System.out.println(Arrays.stream(new Boolean[]{x && y, x || y, (x && !y) || (!x && y), (!x && !y)}).map(i -> i ? "1" : "0").reduce((a,b) -> a + " " + b).get());
    }
}
