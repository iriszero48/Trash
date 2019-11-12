using System;
using static System.Console;
using System.Linq;

namespace SolutionsByProgrammingTask
{
    internal static class Program
    {
        public static void Main(string[] args)
        {
            Enumerable.Range(2008, 2121 - 2007)
                .Select(y => new DateTime(y, 12, 25))
                .Where(d => d.DayOfWeek == DayOfWeek.Sunday)
                .Select(d => d.ToString("yyyy"))
                .ToList()
                .ForEach(WriteLine);
        }
    }
}
