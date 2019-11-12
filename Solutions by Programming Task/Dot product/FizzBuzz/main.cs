using System;
using System.Linq;

namespace SolutionsByProgrammingTask
{
    internal static class Program
    {
        public static void Main(string[] args)
        {
            Enumerable.Range(1, 100).ToList().ForEach(i => Console.WriteLine(i % 5 == 0 ? i % 3 == 0 ? "FizzBuzz" : "Buzz" : i % 3 == 0 ? "Fizz" : i.ToString()));
        }
    }
}
