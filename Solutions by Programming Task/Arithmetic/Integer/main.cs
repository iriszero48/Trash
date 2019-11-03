using System.Linq;
using static System.Console;

namespace SolutionsByProgrammingTask
{
    internal static class Program
    {
        public static void Main(string[] args)
        {
            var input = ReadLine().Split(' ').Select(int.Parse).ToArray();
            var x = input[0];
            var y = input[1];
            WriteLine($"{x} + {y} = {x + y}");
            WriteLine($"{x} - {y} = {x - y}");
            WriteLine($"{x} * {y} = {x * y}");
            WriteLine($"{x} / {y} = {x / y}");
            WriteLine($"{x} % {y} = {x % y}");
            WriteLine($"{x} to the power of {y} = {System.Math.Pow(x, y)}");
        }
    }
}
