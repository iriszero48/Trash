using System.Linq;
using static System.Console;

namespace Program
{
    public class Program
    {
        public static void Main()
        {
            var cin = ReadLine().Split(' ');
            if (cin[0] == "1")
            {
                WriteLine(cin[1]);
                WriteLine(0);
                return;
            }
            int n;
            for (n = 0; Enumerable.Range(1, n + 1).Select(x => x * 2 - 1).Sum() * 2 - 1 < int.Parse(cin[0]); n++) ;
            foreach (var i in Enumerable.Range(2,n - 1).Reverse())
            {
                WriteLine("".PadRight(n - i) + "".PadRight(i * 2 - 1, cin[1][0]));
            }
            foreach (var i in Enumerable.Range(1, n))
            {
                WriteLine("".PadRight(n - i) + "".PadRight(i * 2 - 1, cin[1][0]));
            }

            WriteLine(int.Parse(cin[0]) - (Enumerable.Range(1, n).Select(x => x * 2 - 1).Sum() * 2 - 1));
        }
    }
}
