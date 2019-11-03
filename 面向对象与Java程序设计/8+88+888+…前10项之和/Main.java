import java.util.Collections;

class Main 
{
    public static long Sum(Integer x)
    {
        return x > 0 ? Long.parseLong(String.join("", Collections.nCopies(x, "8"))) + Sum(x - 1) : 0;
    }
    public static void main(String[] args) 
    {
        System.out.println(Sum(10));
    }
}
