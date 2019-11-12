using System.IO;

namespace SolutionsByProgrammingTask
{
    internal static class Program
    {
        public static void Main(string[] args)
        {
            File.Delete("input.txt");
            Directory.Delete("docs");
        }
    }
}
