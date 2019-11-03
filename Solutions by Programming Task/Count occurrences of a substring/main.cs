using static System.Console;

namespace SolutionsByProgrammingTask
{
    internal static class Program
    {
        public static void Main(string[] args)
        {
            var a = ReadLine();
            var b = ReadLine();
            WriteLine(a.Split(b).Length - 1);
        }
    }
}
