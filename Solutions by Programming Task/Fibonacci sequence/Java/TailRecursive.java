public class Main {
    private static int Fib(int x) {
        record Impl() {
            public static int Func(int p, int n, int i) {
                return i == 0 ? n : Func(p + n, p, i - 1);
            }
        }
        return Impl.Func(1, 0, x);
    }
    public static void main(String[] args) {
        System.out.println(Fib(10));
    }
}
