public class Main {
    private static int Fib(int x) {
        if (x == 0) return 0;
        int powTwo;
        if ((x & -x) == x) powTwo = x;
        else {
            var i = x - 1;
            i |= i >> 1;
            i |= i >> 2;
            i |= i >> 4;
            i |= i >> 8;
            i |= i >> 16;
            i += 1;
            powTwo = i / 2;
        }
        int q = 1, r = 1, i = 1, s = 0;
        record Loop(int i, int q, int r, int s) {
            public static Loop Loop1(int i, int q, int r, int s) {
                return new Loop(i * 2, q * q + r * r, r * (q + s), r * r + s * s);
            }

            public static Loop Loop2(int i, int q, int r, int s) {
                return new Loop(i + 1, q + r, q, r);
            }
        }
        while (i < powTwo) {
            var res = Loop.Loop1(i, q, r, s);
            i = res.i;
            q = res.q;
            r = res.r;
            s = res.s;
        }
        while (i < x) {
            var res = Loop.Loop2(i, q, r, s);
            i = res.i;
            q = res.q;
            r = res.r;
            s = res.s;
        }
        return r;
    }

    public static void main(String[] args) {
        System.out.println(Fib(10));
    }
}
