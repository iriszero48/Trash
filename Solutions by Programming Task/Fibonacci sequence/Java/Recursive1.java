public class Main
{
    private static int fib(int x)
    {
        return switch (x)
        {
            case 0 -> 0;
            case 1 -> 1;
            default -> fib(x - 1) + fib(x - 2);
        };
    }

    public static void main(String[] args)
    {
        System.out.println(fib(10));
    }
}
