using System;
using System.Collections;
using System.Collections.Generic;
public class Program
    {
        public static void Main()
        {
            List<string> res = new List<string>();
            var input = Console.ReadLine().Split();
            for (int i = 0; i < Convert.ToInt32(input[0]); ++i) 
            {
                res.Add(ok(Console.ReadLine(), Convert.ToInt32(input[1])));
            }
            foreach(var i in res)
            {
                Console.WriteLine(i);
            }
            Console.Read();
        }
        public static string ok(string input,int max)
        {
            Stack stack = new Stack();
            foreach (char op in input)
            {
                try
                {
                    if (op == 'S')
                    {
                        stack.Push("1");
                        if(stack.Count>max)
                        {
                            throw new OverflowException();
                        }
                    }
                    if(op=='X')
                    {
                        stack.Pop();
                    }
                }
                catch
                {
                    return "NO";
                }
            }
            if (stack.Count == 0) 
            {
                return "YES";
            }
            return "NO";
        }
    }
