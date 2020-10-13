import java.util.LinkedList;
import java.util.List;
import java.util.Scanner;

public class Main
{
    public static void main(String[] args)
    {
        Scanner in = new Scanner(System.in);
        List<String> data = new LinkedList<>();
        while (in.hasNextLine())
        {
            String[] s = in.nextLine().split(" ");
            if (s[0].equals("insert") && !data.contains(s[2]))
            {
                data.add(Integer.parseInt(s[1]) - 1,s[2]);
            }
            else if(s[0].equals("delete") && data.contains(s[1]))
            {
                data.remove(s[1]);
            }
            else if(s[0].equals("search") && data.contains(s[1]))
            {
                System.out.println(data.indexOf(s[1]) + 1);
            }
            else if(s[0].equals("show"))
            {
                System.out.println(String.join(" ", data) + " ");
            }
        }
    }
}
