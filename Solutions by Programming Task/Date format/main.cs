using System;
using static System.Console;

namespace SolutionsByProgrammingTask
{
    internal static class Program
    {
        public static void Main(string[] args)
        {
            var dt = DateTime.Now.Date;
            WriteLine(dt.ToString("yyyy-MM-dd"));
            WriteLine(dt.ToString("dddd, MMMM dd, yyyy"));
        }
    }
}
