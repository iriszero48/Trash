public class Main {
    private static int Fib(int x) {
        return (int) (Math.pow(2, -x)
                * (Math.pow(1 + Math.sqrt(5), x) - Math.pow(-1 + Math.sqrt(5), x) * Math.cos(Math.PI * x))
                / Math.sqrt(5));
    }

    public static void main(String[] args) {
        System.out.println(Fib(10));
    }
}
