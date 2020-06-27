using System;
using System.Collections.Generic;
using System.Linq;

namespace ConsoleApp2
{
    public class Program
    {
        public static void Main()
        {
            string line;
            while ((line = System.Console.ReadLine()) != null)
            {
                var lst = new List<int>();
                var tk = line.Split(',');
                foreach (var s in tk)
                {
                    if (s.Contains("-"))
                    {
                        var ttk = s.Trim().Split('-').Select(x => x.Trim()).Select(int.Parse).ToArray();
                        lst.AddRange(Enumerable.Range(ttk[0], ttk[1] - ttk[0] + 1));
                    }
                    else
                    {
                        lst.Add(int.Parse(s.Trim()));
                    }
                }
                lst.Sort();
                Console.WriteLine(string.Join(",", lst));
            }
        }
    }
}
