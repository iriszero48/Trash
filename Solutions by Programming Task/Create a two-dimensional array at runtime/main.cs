using static System.Console;
using System.Linq;

namespace SolutionsByProgrammingTask
{
    internal static class Program
    {
        public static void Main(string[] args)
        {
            var input = ReadLine().Split(' ').Select(int.Parse).ToArray();
            var arr = new int[input[0], input[1]];
        }
    }
}
