using System.Linq;

namespace SolutionsByProgrammingTask
{
    internal static class Program
    {
        public static void Main(string[] args)
        {
            int[] a = {1, 2, 3};
            int[] b = {4, 5, 6};
            var c = a.Concat(b).ToArray();
        }
    }
}
