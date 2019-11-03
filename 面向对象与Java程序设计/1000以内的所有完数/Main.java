import java.util.stream.IntStream;

class Main
{
    private static void PerfectNumber()
    {
        IntStream.range(1, 1000).filter(f -> IntStream.rangeClosed(1, f / 2).filter(sf -> f % sf == 0).sum() == f).forEach(System.out::println);
    }
    public static void main(String[] args)
    {
        PerfectNumber();
    }
}
