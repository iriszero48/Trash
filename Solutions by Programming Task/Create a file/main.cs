using System.IO;

namespace SolutionsByProgrammingTask
{
    internal static class Program
    {
        public static void Main(string[] args)
        {
            File.Create("output.txt");
            Directory.CreateDirectory("docs");
        }
    }
}
