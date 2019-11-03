using System;
using System.Collections.Generic;

namespace SolutionsByProgrammingTask
{
    internal static class Program
    {
        public static void Main(string[] args)
        {
            var dictionary = new Dictionary<string, int>()
            {
                {"one", 1},
                {"two", 2},
                {"three", 3}
            };
            foreach (var i in dictionary)
            {
                Console.WriteLine($"{i.Key} => {i.Value}");
            }
        }
    }
}
