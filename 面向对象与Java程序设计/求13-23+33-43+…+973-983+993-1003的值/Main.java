class Main 
{
    public static int Sum(Integer x)
    {
        return x > 0 ? (x % 2 == 0 ? -1 : 1) * (x * 10 + 3) + Sum(x - 1) : 0;
    }
    public static void main(String[] args) 
    {
        System.out.println(Sum(100));
    }
}
