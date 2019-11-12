using static System.Console;
using System.Linq;

namespace SolutionsByProgrammingTask
{
    internal static class Program
    {
        public static void Main(string[] args)
        {
            Enumerable.Range('a', 26).ToList().ForEach(i => Write((char)i));
        }
    }
}
