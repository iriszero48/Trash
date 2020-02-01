using static System.Console;
using System.Linq;

namespace Catamorphism
{
    class Catamorphism
    {
        static void Main(string[] args)
        {
            var lst = Enumerable.Range(1, 5);
            WriteLine(lst.Aggregate((a, b) => a + b));
            WriteLine(lst.Aggregate(0, (a, b) => a + b));
            WriteLine(lst.Reverse().Aggregate((a, b) => a + b));
            WriteLine(lst.Reverse().Aggregate(0, (a, b) => a + b));
        }
    }
}
