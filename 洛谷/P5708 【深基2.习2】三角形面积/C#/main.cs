using System;
using System.Linq;
using static System.Console;

namespace Program
{
    public class Program
    {
        public static void Main()
        {
            var a = ReadLine().Split(' ').Select(double.Parse);
            var p = 0.5 * a.Sum();
            WriteLine(Math.Sqrt(p * a.Select(x => p -x).Aggregate((x,y) => x * y)).ToString("F1"));
        }
    }
}
