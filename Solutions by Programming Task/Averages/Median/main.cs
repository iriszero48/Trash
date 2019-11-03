using System.Linq;

namespace SolutionsByProgrammingTask
{
    internal static class Program
    {
        public static void Main(string[] args)
        {
            int[] a = {1, 2, 3};
            var b = a.OrderBy(i => i).ToArray();
            if (a.Length%2==1)
            {
                var median = b[b.Length/2+1];
            }
            else
            {
                var median = b.Skip(b.Length / 2 - 1).Take(2).Average();
            }
        }
    }
}
