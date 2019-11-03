using System.Linq;
using static System.Math;

namespace SolutionsByProgrammingTask
{
    internal static class Program
    {
        public static void Main(string[] args)
        {
            int[] a = {1, 2, 3};
            var rootMeanSquare = Sqrt(a.Sum(i => i * i) / a.Length);
        }
    }
}
