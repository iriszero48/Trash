import java.util.Arrays;
public class Main 
{
    public static void main(String[] args) 
    {
        String s = "this is a test of java";
        System.out.println(s.split("s").length - 1);
        System.out.println(s.split("is").length - 1);
        System.out.println(Arrays.asList(s.split(" ")).stream().filter(i -> i.equals("is")).count());
        System.out.println(new StringBuffer(s).reverse().toString());
    }
}
