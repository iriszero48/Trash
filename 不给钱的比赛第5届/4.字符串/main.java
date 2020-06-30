import java.util.*;

public class Main
{
    public static void main(String... args)
    {
        System.out.println(Arrays.stream(new Scanner(System.in).nextLine().split("([ ])\\1*")).map(x -> x.substring(0, 1).toUpperCase() + x.substring(1)).map(x ->
        {
            if (x.matches("[A-Za-z]+")) return x;
            else
            {
                List<String> lst = new ArrayList<>();
                String[] ss = x.split("\\d+");
                int len = 0;
                for (int i = 0; i < ss.length; i++)
                {
                    if (!ss[i].equals("")) lst.add(ss[i]);
                    len += ss[i].length();
                    if (i != ss.length - 1) lst.add(x.substring(len, len + x.substring(len).indexOf(ss[i + 1])));
                }
                return String.join("_", lst);
            }
        }).reduce((a, b) -> a + " " + b).get());
    }
}
