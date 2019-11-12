using System;
using System.Collections.Generic;
public class Program
    {
        public static void Main()
        {
            string input = Console.ReadLine();
            List<string> all = new List<string>();
            foreach(string x in input.Split(' '))
            {
                if(x!="")
                {
                    all.Add(x);
                }
            }
            Console.Write(all.Count);
        }
    }
