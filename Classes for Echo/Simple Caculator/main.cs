using System;
using System.Text.RegularExpressions;
using static System.Console;
using static System.Convert;

namespace SolutionsByProgrammingTask
{
    internal static class Program
    {
        private static string Oper2(string input, char key, Func<decimal, decimal, decimal> f)
        {
            if (input[0] == '-')
            {
                input = '0' + input;
            }

            decimal a = 0;
            decimal b = 0;
            for (var i = 0; i < input.Length; i++)
            {
                if (input[i] != key) continue;
                int j = i - 1, ai = -1, bi = -1;
                for (; j >= 0; j--)
                {
                    if (!Regex.IsMatch(input[j].ToString(), @"[+\-*\/]")) continue;
                    ai = j + 1;
                    a = ToDecimal(input.Substring(ai, i - ai));
                    break;
                }

                if (ai == -1)
                {
                    ai = 0;
                    a = ToDecimal(input.Substring(ai, i));
                }

                for (j = i + 1; j < input.Length; j++)
                {
                    if (!Regex.IsMatch(input[j].ToString(), @"[+\-*\/]")) continue;
                    bi = j - 1;
                    b = ToDecimal(input.Substring(i + 1, bi - i));
                    break;
                }

                if (bi == -1)
                {
                    bi = input.Length - 1;
                    b = ToDecimal(input.Substring(i + 1, bi - i));
                }

                input = input.Replace(input.Substring(ai, bi - ai + 1), f(a, b).ToString());
                i = 0;
            }

            return input;
        }

        private static string Oper1(string input, string key, Func<decimal, decimal> f)
        {
            decimal a = 0;
            decimal b = 0;
            for (var i = 0; i < input.Length - (key.Length + 2); i++)
            {
                if (input.Substring(i, key.Length) != key) continue;
                int j = i - 1, bi = -1;

                int count = 0;
                for (j = i + key.Length + 1; j < input.Length; j++)
                {
                    if (input[j] == '(')
                        ++count;
                    else if (input[j] == ')' && count == 0)
                    {
                        bi = j;
                        break;
                    }
                    else if (input[j] == ')') --count;
                }

                input = input.Replace(input.Substring(i, bi - i + 1),
                    f(ToDecimal(FA(input.Substring(i + key.Length + 1, bi - (i + key.Length + 1))))).ToString());
                i = 0;
            }

            return input;
        }

        public static string FF(string input) =>
            Oper1(
                Oper1(Oper1(input, "tan", x => (decimal) System.Math.Tan(ToDouble(x))), "cos",
                    x => (decimal) System.Math.Cos(ToDouble(x))), "sin", x => (decimal) System.Math.Sin(ToDouble(x)));

        public static string F4(string input) =>
            Oper2(Oper2(Oper2(Oper2(input, '*', (a, b) => a * b), '/', (a, b) => a / b), '+', (a, b) => a + b), '-',
                (a, b) => a - b);

        public static string FA(string input) => F4(FF(input));

        public static void Main(string[] args)
        {
            var input = ReadLine();
            WriteLine(FA(input));
            ReadLine();
        }
    }
}
