import java.math.BigInteger;
import java.util.Arrays;
import java.util.Scanner;
import java.util.stream.Collectors;

public class Main
{
    public static void main(String[] args)
    {
        int[] cin = Arrays.stream(new Scanner(System.in).nextLine().split(" ")).mapToInt(Integer::parseInt).sorted().toArray();
        int gcd = BigInteger.valueOf(cin[0]).gcd(BigInteger.valueOf(cin[2])).intValue();
        System.out.println(Arrays.stream(new int[] {cin[0], cin[2]}).mapToObj(x -> x / gcd + "").collect(Collectors.joining("/")));
    }
}
