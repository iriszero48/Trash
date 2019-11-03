import java.util.stream.IntStream;

class Main 
{
    public static int Sum(Integer n)
    {
        return IntStream.range(1, n).filter(f -> IntStream.rangeClosed(1, f).sum() < n).max().getAsInt();
    }
    public static void main(String[] args) 
    {
        System.out.println(Sum(8888));
    }
}
