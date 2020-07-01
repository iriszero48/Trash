import java.util.*;
import java.util.function.Function;

public class Main
{
    public static void main(String... args)
    {
        Scanner scanner = new Scanner(System.in);
        int m = scanner.nextInt();
        int n = scanner.nextInt();
        final int me = 2 * m, ne = 2 * n;
        Function<String, Integer> f = x -> { System.out.print(x); return 0; };
        for (int i = 0; i < me + 1; i++)
        {
            for (int j = 0; j < ne + 1; j++)
            {
                if (i == 0)
                {
                    if (j == 0) f.apply("┌");
                    else if (j == ne) f.apply("┐");
                    else if ((j & 1) == 0) f.apply("┬");
                    else f.apply("─");
                }
                else if (i == me)
                {
                    if (j == 0) f.apply("└");
                    else if (j == ne) f.apply("┘");
                    else if ((j & 1) == 0) f.apply("┴");
                    else f.apply("─");
                }
                else if ((i & 1) == 1)
                {
                    f.apply((j & 1) == 0 ? "│" : "\u3000");
                }
                else
                {
                    if (j == 0) f.apply("├");
                    else if (j == ne) f.apply("┤");
                    else if ((j & 1) == 0) f.apply("┼");
                    else f.apply("─");
                }
            }
            f.apply("\n");
        }
    }
}
