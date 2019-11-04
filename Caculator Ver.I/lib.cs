using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.IO;

namespace CaculatorVersionIDatabase
{
    public abstract class BaseCaculator
    {
        private byte times;
        public byte Times
        {
            get
            {
                return times;
            }
            set
            {
                Trace.Assert(times < 255, "Invalid Syntax");
                times = value;
            }
        }
        private double Function(double t)
        {
            return Math.Exp(-Math.Pow(t, 2));
        }
        private double DefiniteIntegral(double x)
        {
            double total = 0;
            for (double i = 0; i < x; i += x / 1000000)
                total += ((Function(i) + Function(i + x / 1000000)) * x / 1000000) / 2;
            return total;
        }
        private double Erf(string x)
        {
            return 2 * DefiniteIntegral(Convert.ToDouble(x)) / Math.Sqrt(Math.PI);
        }
        private double Factorial(double n)
        {
            double times = 0;
            double total = 1;
            if (n == 0)
                return 1;
            while (times < n)
            {
                ++times;
                total *= times;
            }
            return total;
        }
        private double DoubleFactorial(double n)
        {
            double times = 0;
            double total = 1;
            if (n == 0)
                return 1;
            if(n%2==0&&n>0)
            {
                while (times < n)
                {
                    times += 2;
                    total *= times;
                }
                return total;
            }
            else if (n % 2 != 0 && n > 0)
            {
                times = 1;
                while (times < n)
                {
                    times += 2;
                    total *= times;
                }
                return total;
            }
            else if(n%2!=0&&n<0)
            {
                times = 1;
                while (times < Math.Abs(n))
                {
                    times += 2;
                    total *= times;
                }
                return Math.Pow(-1, n) / total;
            }
            else
                Trace.Assert(false, "Invalid Syntax");
            return 0;
        }
        private double GreatestCommonDivisor(double a,double b)
        {
            double gcd = 1;
            double min = a > b ? b : a;
            for (double i = min; i >= 1; i--)
            {
                if (a % i == 0 && b % i == 0)
                {
                    gcd = i;
                    break;
                }
            }
            return gcd;
        }
        private double LowestCommonMultiple(double a,double b)
        {
            double lcm = a * b;
            double max = a > b ? a : b;
            for (double i = max, len = a * b; i <= len; i++)
            {
                if (i % a == 0 && i % b == 0)
                {
                    lcm = i;
                    break;
                }
            }
            return lcm;
        }
        #region 1stVersionPart1
        /// <summary>
        /// 1st version result:false
        /// delegate string FunctionA(string aa);
        /// delegate string FunctionAB(string aba,string abb);
        /// delegate string FunctionAS(string asa,string ass);
        /// string aa, aba, abb, asa, ass;
        /// FunctionA resultA;
        /// FunctionAB resultAB;
        /// FunctionAS resultAS;
        /// private string Lg(string aa)
        /// {
        ///     return Convert.ToString(Math.Log10(Convert.ToDouble(aa)));
        /// }
        /// private string Log(string aba,string abb)
        /// {
        ///     return Convert.ToString(Math.Log10(Convert.ToDouble(abb)) / Math.Log10(Convert.ToDouble(aba)));
        /// }
        /// private string Sin(string asa, string ass)
        /// {
        /// if(Convert.ToBoolean(ass))
        ///     return Convert.ToString(Math.Sin(Convert.ToDouble(asa)));
        /// else
        ///     return Convert.ToString(Math.Sin(Convert.ToDouble(asa) * Math.PI / 180));
        /// }
        /// </summary>
        /// <returns></returns>
        #endregion
        public virtual void CheckFunction(ref List<string>numbersAndSymbolics)
        {
            Times = 0;
            while(Times<numbersAndSymbolics.Count)
            {
                switch(numbersAndSymbolics[Times])
                {
                    case "lg":
                        #region 1stVersionPart2
                        /// <summary>
                        /// 1st version result:false
                        /// aa = numbersAndSymbolics[Times + 1];
                        /// resultA = Lg;
                        /// Console.WriteLine(resultA);
                        /// numbersAndSymbolics[Times] = resultA;
                        /// </summary>
                        /// <returns></returns>
                        #endregion
                        numbersAndSymbolics[Times] = Convert.ToString(Math.Log10(Convert.ToDouble(numbersAndSymbolics[Times + 1])));
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "sinh":
                        numbersAndSymbolics[Times] = Convert.ToString(Math.Sinh(Convert.ToDouble(numbersAndSymbolics[Times + 1])));
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "cosh":
                        numbersAndSymbolics[Times] = Convert.ToString(Math.Cosh(Convert.ToDouble(numbersAndSymbolics[Times + 1])));
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "tanh":
                        numbersAndSymbolics[Times] = Convert.ToString(Math.Tanh(Convert.ToDouble(numbersAndSymbolics[Times + 1])));
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "coth":
                        numbersAndSymbolics[Times] = Convert.ToString(1 / Math.Tanh(Convert.ToDouble(numbersAndSymbolics[Times + 1])));
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "sech":
                        numbersAndSymbolics[Times] = Convert.ToString(1 / Math.Cosh(Convert.ToDouble(numbersAndSymbolics[Times + 1])));
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "csch":
                        numbersAndSymbolics[Times] = Convert.ToString(1 / Math.Sinh(Convert.ToDouble(numbersAndSymbolics[Times + 1])));
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "abs":
                        numbersAndSymbolics[Times] = Convert.ToString(Math.Abs(Convert.ToDouble(numbersAndSymbolics[Times + 1])));
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "arcosh":
                        numbersAndSymbolics[Times] = Convert.ToString(Math.Log(Convert.ToDouble(numbersAndSymbolics[Times + 1]) + Math.Sqrt(Math.Pow(Convert.ToDouble(numbersAndSymbolics[Times + 1]), 2) - 1)));
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "arsinh":
                        numbersAndSymbolics[Times] = Convert.ToString(Math.Log(Convert.ToDouble(numbersAndSymbolics[Times + 1]) + Math.Sqrt(Math.Pow(Convert.ToDouble(numbersAndSymbolics[Times + 1]), 2) + 1)));
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "artanh":
                        numbersAndSymbolics[Times] = Convert.ToString(Math.Log((1 + Convert.ToDouble(numbersAndSymbolics[Times + 1])) / (1 - Convert.ToDouble(numbersAndSymbolics[Times + 1])) / 2));
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "arcoth":
                        numbersAndSymbolics[Times] = Convert.ToString(Math.Log((1 + Convert.ToDouble(numbersAndSymbolics[Times + 1])) / (Convert.ToDouble(numbersAndSymbolics[Times + 1]) - 1) / 2));
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "+arsech":
                        numbersAndSymbolics[Times] = Convert.ToString(Math.Log(1 + Math.Sqrt(1 - Math.Pow(Convert.ToDouble(numbersAndSymbolics[Times + 1]), 2)) / Convert.ToDouble(numbersAndSymbolics[Times + 1])));
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "-arsech":
                        numbersAndSymbolics[Times] = Convert.ToString(-Math.Log(1 + Math.Sqrt(1 - Math.Pow(Convert.ToDouble(numbersAndSymbolics[Times + 1]), 2)) / Convert.ToDouble(numbersAndSymbolics[Times + 1])));
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "arcsch":
                        if (Convert.ToDouble(numbersAndSymbolics[Times + 1]) < 0)
                            numbersAndSymbolics[Times] = Convert.ToString(Math.Log(1 - Math.Sqrt(1 + Math.Pow(Convert.ToDouble(numbersAndSymbolics[Times + 1]), 2)) / Convert.ToDouble(numbersAndSymbolics[Times + 1])));
                        else
                            numbersAndSymbolics[Times] = Convert.ToString(Math.Log(1 + Math.Sqrt(1 + Math.Pow(Convert.ToDouble(numbersAndSymbolics[Times + 1]), 2)) / Convert.ToDouble(numbersAndSymbolics[Times + 1])));
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "dec":
                        numbersAndSymbolics[Times] = Convert.ToString(Convert.ToDouble(numbersAndSymbolics[Times + 1]) * 180 / Math.PI);
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "erf":
                        numbersAndSymbolics[Times] = Convert.ToString(Erf(numbersAndSymbolics[Times + 1]));
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "erfc":
                        numbersAndSymbolics[Times] = Convert.ToString(1 - Erf(numbersAndSymbolics[Times + 1]));
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "!":
                        numbersAndSymbolics[Times] = Convert.ToString(Factorial(Convert.ToDouble(numbersAndSymbolics[Times + 1])));
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "!!":
                        numbersAndSymbolics[Times] = Convert.ToString(DoubleFactorial(Convert.ToDouble(numbersAndSymbolics[Times + 1])));
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "rad":
                        numbersAndSymbolics[Times] = Convert.ToString(Convert.ToDouble(numbersAndSymbolics[Times + 1]) * Math.PI / 180);
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "log":
                        #region 1stVersionPart3
                        /// <summary>
                        /// 1st version result:false
                        /// aba = numbersAndSymbolics[Times + 1];
                        /// abb = numbersAndSymbolics[Times + 2];
                        /// resultAB = Log;
                        /// numbersAndSymbolics[Times] = Convert.ToString(resultAB);
                        /// </summary>
                        /// <returns></returns>
                        #endregion
                        numbersAndSymbolics[Times] = Convert.ToString(Math.Log10(Convert.ToDouble(numbersAndSymbolics[Times + 2])) / Math.Log10(Convert.ToDouble(numbersAndSymbolics[Times + 1])));
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "pow":
                        numbersAndSymbolics[Times] = Convert.ToString(Math.Pow(Convert.ToDouble(numbersAndSymbolics[Times + 1]), (Convert.ToDouble(numbersAndSymbolics[Times + 2]))));
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "ncr":
                        numbersAndSymbolics[Times] = Convert.ToString(Factorial(Convert.ToDouble(numbersAndSymbolics[Times + 1])) / (Factorial(Convert.ToDouble(numbersAndSymbolics[Times + 2])) * Factorial(Convert.ToDouble(numbersAndSymbolics[Times + 1]) - Convert.ToDouble(numbersAndSymbolics[Times + 2]))));
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "npr":
                        numbersAndSymbolics[Times] = Convert.ToString(Factorial(Convert.ToDouble(numbersAndSymbolics[Times + 1])) / Factorial(Convert.ToDouble(numbersAndSymbolics[Times + 1]) - Convert.ToDouble(numbersAndSymbolics[Times + 2])));
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "mod":
                        numbersAndSymbolics[Times] = Convert.ToString(Convert.ToDouble(numbersAndSymbolics[Times])%(Convert.ToDouble(numbersAndSymbolics[Times + 2])));
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "lcm":
                        numbersAndSymbolics[Times] = Convert.ToString(LowestCommonMultiple(Convert.ToDouble(numbersAndSymbolics[Times + 1]), (Convert.ToDouble(numbersAndSymbolics[Times + 2]))));
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "gcd":
                        numbersAndSymbolics[Times] = Convert.ToString(GreatestCommonDivisor(Convert.ToDouble(numbersAndSymbolics[Times + 1]), (Convert.ToDouble(numbersAndSymbolics[Times + 2]))));
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "sin":
                        #region 1stVersionPart4
                        /// <summary>
                        /// 1st version result:false
                        /// asa = numbersAndSymbolics[Times + 1];
                        /// ass = numbersAndSymbolics[Times + 2];
                        /// resultAS = Sin;
                        /// numbersAndSymbolics[Times] = Convert.ToString(resultAS);
                        /// </summary>
                        /// <returns></returns>
                        #endregion
                        Trace.Assert(numbersAndSymbolics[Times + 2] == "d" || numbersAndSymbolics[Times + 2] == "r", "Invalid Syntax");
                        if (numbersAndSymbolics[Times + 2] == "d")
                            numbersAndSymbolics[Times] = Convert.ToString(Math.Sin(Convert.ToDouble(numbersAndSymbolics[Times + 1])));
                        else
                            numbersAndSymbolics[Times] = Convert.ToString(Math.Sin(Convert.ToDouble(numbersAndSymbolics[Times + 1]) * Math.PI / 180));
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "cos":
                        Trace.Assert(numbersAndSymbolics[Times + 2] == "d" || numbersAndSymbolics[Times + 2] == "r", "Invalid Syntax");
                        if (numbersAndSymbolics[Times + 2] == "d")
                            numbersAndSymbolics[Times] = Convert.ToString(Math.Cos(Convert.ToDouble(numbersAndSymbolics[Times + 1])));
                        else
                            numbersAndSymbolics[Times] = Convert.ToString(Math.Cos(Convert.ToDouble(numbersAndSymbolics[Times + 1]) * Math.PI / 180));
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "tan":
                        Trace.Assert(numbersAndSymbolics[Times + 2] == "d" || numbersAndSymbolics[Times + 2] == "r", "Invalid Syntax");
                        if (numbersAndSymbolics[Times + 2] == "d")
                            numbersAndSymbolics[Times] = Convert.ToString(Math.Tan(Convert.ToDouble(numbersAndSymbolics[Times + 1])));
                        else
                            numbersAndSymbolics[Times] = Convert.ToString(Math.Tan(Convert.ToDouble(numbersAndSymbolics[Times + 1]) * Math.PI / 180));
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "cot":
                        Trace.Assert(numbersAndSymbolics[Times + 2] == "d" || numbersAndSymbolics[Times + 2] == "r", "Invalid Syntax");
                        if (numbersAndSymbolics[Times + 2] == "d")
                            numbersAndSymbolics[Times] = Convert.ToString(1 / Math.Tan(Convert.ToDouble(numbersAndSymbolics[Times + 1])));
                        else
                            numbersAndSymbolics[Times] = Convert.ToString(1 / Math.Tan((Convert.ToDouble(numbersAndSymbolics[Times + 1]) * Math.PI / 180)));
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "sec":
                        Trace.Assert(numbersAndSymbolics[Times + 2] == "d" || numbersAndSymbolics[Times + 2] == "r", "Invalid Syntax");
                        if (numbersAndSymbolics[Times + 2] == "d")
                            numbersAndSymbolics[Times] = Convert.ToString(1 / Math.Cos(Convert.ToDouble(numbersAndSymbolics[Times + 1])));
                        else
                            numbersAndSymbolics[Times] = Convert.ToString(1 / Math.Cos(Convert.ToDouble(numbersAndSymbolics[Times + 1]) * Math.PI / 180));
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "csc":
                        Trace.Assert(numbersAndSymbolics[Times + 2] == "d" || numbersAndSymbolics[Times + 2] == "r", "Invalid Syntax");
                        if (numbersAndSymbolics[Times + 2] == "d")
                            numbersAndSymbolics[Times] = Convert.ToString(1 / Math.Sin(Convert.ToDouble(numbersAndSymbolics[Times + 1])));
                        else
                            numbersAndSymbolics[Times] = Convert.ToString(1 / Math.Sin(Convert.ToDouble(numbersAndSymbolics[Times + 1]) * Math.PI / 180));
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "asin":
                        Trace.Assert(numbersAndSymbolics[Times + 2] == "d" || numbersAndSymbolics[Times + 2] == "r", "Invalid Syntax");
                        if (numbersAndSymbolics[Times + 2] == "d")
                            numbersAndSymbolics[Times] = Convert.ToString(Math.Asin(Convert.ToDouble(numbersAndSymbolics[Times + 1])));
                        else
                            numbersAndSymbolics[Times] = Convert.ToString(Math.Asin(Convert.ToDouble(numbersAndSymbolics[Times + 1]) * Math.PI / 180));
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "acos":
                        Trace.Assert(numbersAndSymbolics[Times + 2] == "d" || numbersAndSymbolics[Times + 2] == "r", "Invalid Syntax");
                        if (numbersAndSymbolics[Times + 2] == "d")
                            numbersAndSymbolics[Times] = Convert.ToString(Math.Acos(Convert.ToDouble(numbersAndSymbolics[Times + 1])));
                        else
                            numbersAndSymbolics[Times] = Convert.ToString(Math.Acos(Convert.ToDouble(numbersAndSymbolics[Times + 1]) * Math.PI / 180));
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "atan":
                        Trace.Assert(numbersAndSymbolics[Times + 2] == "d" || numbersAndSymbolics[Times + 2] == "r", "Invalid Syntax");
                        if (numbersAndSymbolics[Times + 2] == "d")
                            numbersAndSymbolics[Times] = Convert.ToString(Math.Atan(Convert.ToDouble(numbersAndSymbolics[Times + 1])));
                        else
                            numbersAndSymbolics[Times] = Convert.ToString(Math.Atan(Convert.ToDouble(numbersAndSymbolics[Times + 1]) * Math.PI / 180));
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "acot":
                        Trace.Assert(numbersAndSymbolics[Times + 2] == "d" || numbersAndSymbolics[Times + 2] == "r", "Invalid Syntax");
                        if (numbersAndSymbolics[Times + 2] == "d")
                            numbersAndSymbolics[Times] = Convert.ToString(Math.Atan(1 / Convert.ToDouble(numbersAndSymbolics[Times + 1])));
                        else
                            numbersAndSymbolics[Times] = Convert.ToString(Math.Atan(1 / (Convert.ToDouble(numbersAndSymbolics[Times + 1]) * Math.PI / 180)));
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "asec":
                        Trace.Assert(numbersAndSymbolics[Times + 2] == "d" || numbersAndSymbolics[Times + 2] == "r", "Invalid Syntax");
                        if (numbersAndSymbolics[Times + 2] == "d")
                            numbersAndSymbolics[Times] = Convert.ToString(Math.Acos(1 / Convert.ToDouble(numbersAndSymbolics[Times + 1])));
                        else
                            numbersAndSymbolics[Times] = Convert.ToString(Math.Acos(1 / (Convert.ToDouble(numbersAndSymbolics[Times + 1]) * Math.PI / 180)));
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "acsc":
                        Trace.Assert(numbersAndSymbolics[Times + 2] == "d" || numbersAndSymbolics[Times + 2] == "r", "Invalid Syntax");
                        if (numbersAndSymbolics[Times + 2] == "d")
                            numbersAndSymbolics[Times] = Convert.ToString(Math.Asin(1 / Convert.ToDouble(numbersAndSymbolics[Times + 1])));
                        else
                            numbersAndSymbolics[Times] = Convert.ToString(Math.Asin(1 / (Convert.ToDouble(numbersAndSymbolics[Times + 1]) * Math.PI / 180)));
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        numbersAndSymbolics.RemoveAt(Times + 1);
                        break;
                    case "pi":
                        numbersAndSymbolics[Times] = Convert.ToString(Math.PI);
                        break;
                    case "phi":
                        numbersAndSymbolics[Times] = Convert.ToString(1.6180339887498948482);
                        break;
                    case "e":
                        numbersAndSymbolics[Times] = Convert.ToString(Math.E);
                        break;
                    case "k":
                        numbersAndSymbolics[Times] = Convert.ToString(9E9);
                        break;
                    case "c":
                        numbersAndSymbolics[Times] = Convert.ToString(299792458);
                        break;
                }
                ++Times;
            }
        }
        public virtual string[] CollectNumbersAndSymbolics()
        {
            char[] separator = { ' ' };
            string inputNumbersAndSymbolics;
            inputNumbersAndSymbolics = Console.ReadLine();
            return inputNumbersAndSymbolics.Split(separator);
        }
        protected internal bool CheckSymbolic(char symbolic,List<string>numbersAndSymbolics)
        {
            Times = 0;
            while(Times<numbersAndSymbolics.Count)
            {
                if (numbersAndSymbolics[Times] == Convert.ToString(symbolic))
                    return true;
                ++Times;
            }
            return false;
        }
        public virtual void Caculator(ref List<string>numbersAndSymbolics)
        {
            CheckFunction(ref numbersAndSymbolics);
            while(CheckSymbolic('/',numbersAndSymbolics))
            {
                numbersAndSymbolics[Times - 1] =Convert.ToString(Convert.ToDouble(numbersAndSymbolics[Times - 1]) / Convert.ToDouble(numbersAndSymbolics[Times + 1]));
                numbersAndSymbolics.RemoveAt(Times);
                numbersAndSymbolics.RemoveAt(Times);
            }
            while (CheckSymbolic('*', numbersAndSymbolics))
            {
                numbersAndSymbolics[Times - 1] = Convert.ToString(Convert.ToDouble(numbersAndSymbolics[Times - 1]) * Convert.ToDouble(numbersAndSymbolics[Times + 1]));
                numbersAndSymbolics.RemoveAt(Times);
                numbersAndSymbolics.RemoveAt(Times);
            }
            while (CheckSymbolic('-', numbersAndSymbolics))
            {
                numbersAndSymbolics[Times - 1] = Convert.ToString(Convert.ToDouble(numbersAndSymbolics[Times - 1]) - Convert.ToDouble(numbersAndSymbolics[Times + 1]));
                numbersAndSymbolics.RemoveAt(Times);
                numbersAndSymbolics.RemoveAt(Times);
            }
            while (CheckSymbolic('+', numbersAndSymbolics))
            {
                numbersAndSymbolics[Times - 1] = Convert.ToString(Convert.ToDouble(numbersAndSymbolics[Times - 1]) + Convert.ToDouble(numbersAndSymbolics[Times + 1]));
                numbersAndSymbolics.RemoveAt(Times);
                numbersAndSymbolics.RemoveAt(Times);
            }
        }
    }
    public sealed class Statistics:BaseCaculator
    {
        public override string[] CollectNumbersAndSymbolics()
        {
            char[] separator = { ' ', ',' };
            string inputNumbersAndSymbolics;
            inputNumbersAndSymbolics = Console.ReadLine();
            return inputNumbersAndSymbolics.Split(separator);
        }
        private void ChangeType(List<string>numbersAndSymbolics,ref List<double>numbers)
        {
            byte th = 0;
            while(th<numbersAndSymbolics.Count)
            {
                numbers.Add(Convert.ToDouble(numbersAndSymbolics[th]));
                ++th;
            }
        }
        public void StatisticsStart(ref List<double>numbers)
        {
            List<string> numbersAndSymbolics = new List<string>();
            foreach (string numberAndSymbolic in CollectNumbersAndSymbolics())
                numbersAndSymbolics.Add(numberAndSymbolic);
            Caculator(ref numbersAndSymbolics);
            ChangeType(numbersAndSymbolics, ref numbers);
        }
        public double Median(List<double> numbers)
        {
            List<double> combinationNumbers = new List<double>();
            var combinationResults = numbers.OrderBy(number => number);
            foreach (var combinationNumber in combinationResults)
                combinationNumbers.Add(combinationNumber);
            if (combinationNumbers.Count % 2 == 0)
                return (combinationNumbers[combinationNumbers.Count / 2 - 1] + combinationNumbers[combinationNumbers.Count / 2]) / 2;
            else
                return combinationNumbers[(combinationNumbers.Count + 1) / 2 - 1];
        }
        #region 3rdVersionPart1
        /// <summary>
        /// using Ling
        /// public double Total(List<double>numbers)
        /// {
        ///     double total = 0;
        ///     foreach (double number in numbers)
        ///         total += number;
        ///     return total;
        /// }
        /// public double Mean(List<double>numbers)
        /// {
        ///     return Total(numbers) / numbers.Count;
        /// }
        /// </summary>
        /// <returns></returns>
        #endregion
    }
    public sealed class Complex:BaseCaculator
    {
        public override void CheckFunction(ref List<string> numbersAndSymbolics)
        {
            Times = 0;
            double conj = 0;
            while(Times<numbersAndSymbolics.Count)
            {
                if (numbersAndSymbolics[Times]=="pow")
                {
                    conj = Math.Sqrt(Math.Pow(Convert.ToDouble(numbersAndSymbolics[Times + 1]), 2) + Math.Pow(Convert.ToDouble(numbersAndSymbolics[Times + 2]), 2));
                    numbersAndSymbolics[Times] = Convert.ToString(Math.Cos(Convert.ToDouble(numbersAndSymbolics[Times + 3]) * Math.Acos(Convert.ToDouble(numbersAndSymbolics[Times + 1]) / conj)) * conj);
                    numbersAndSymbolics[Times+1] = Convert.ToString(Math.Sin(Convert.ToDouble(numbersAndSymbolics[Times + 3]) * Math.Asin(Convert.ToDouble(numbersAndSymbolics[Times + 2]) / conj)) * conj);
                    numbersAndSymbolics.RemoveAt(Times + 2);
                    numbersAndSymbolics.RemoveAt(Times + 2);
                }
                if(numbersAndSymbolics[Times]=="arg")
                {
                    conj = Math.Sqrt(Math.Pow(Convert.ToDouble(numbersAndSymbolics[Times + 1]), 2) + Math.Pow(Convert.ToDouble(numbersAndSymbolics[Times + 2]), 2));
                    numbersAndSymbolics[Times] = Convert.ToString(Math.Acos(Convert.ToDouble(numbersAndSymbolics[Times + 1]) / conj));
                    numbersAndSymbolics[Times + 1] = Convert.ToString(0);
                    numbersAndSymbolics.RemoveAt(Times + 2);
                }
                if(numbersAndSymbolics[Times]=="conj")
                {
                    conj = Math.Sqrt(Math.Pow(Convert.ToDouble(numbersAndSymbolics[Times + 1]), 2) + Math.Pow(Convert.ToDouble(numbersAndSymbolics[Times + 2]), 2));
                    numbersAndSymbolics[Times] = Convert.ToString(conj);
                    numbersAndSymbolics[Times + 1] = Convert.ToString(0);
                    numbersAndSymbolics.RemoveAt(Times + 2);
                }
                ++Times;
            }
        }
        #region 2ndVersionPart1
        /// <summary>
        /// 2nd version complex caculator result:false
        /// public override string[] CollectNumbersAndSymbolics()
        /// {
        ///     char[] separator = { ',', ' ' };
        ///     string inputNumbersAndSymbolics;
        ///     inputNumbersAndSymbolics = Console.ReadLine();
        ///     return inputNumbersAndSymbolics.Split(separator);
        /// }
        /// </summary>
        /// <returns></returns>
        #endregion
        public override void Caculator(ref List<string> numbersAndSymbolics)
        {
            double temporaryA, temporaryB;
            CheckFunction(ref numbersAndSymbolics);
            while (CheckSymbolic('/', numbersAndSymbolics))
            {
                temporaryA = (Convert.ToDouble(numbersAndSymbolics[Times - 2]) * Convert.ToDouble(numbersAndSymbolics[Times + 1]) + Convert.ToDouble(numbersAndSymbolics[Times - 1]) * Convert.ToDouble(numbersAndSymbolics[Times + 2])) / (Math.Pow(Convert.ToDouble(numbersAndSymbolics[Times + 1]), 2) + Math.Pow(Convert.ToDouble(numbersAndSymbolics[Times + 2]), 2));
                temporaryB = (Convert.ToDouble(numbersAndSymbolics[Times - 1]) * Convert.ToDouble(numbersAndSymbolics[Times + 1]) - Convert.ToDouble(numbersAndSymbolics[Times - 2]) * Convert.ToDouble(numbersAndSymbolics[Times + 2])) / (Math.Pow(Convert.ToDouble(numbersAndSymbolics[Times + 1]), 2) + Math.Pow(Convert.ToDouble(numbersAndSymbolics[Times + 2]), 2));
                numbersAndSymbolics[Times - 2] = Convert.ToString(temporaryA);
                numbersAndSymbolics[Times - 1] = Convert.ToString(temporaryB);
                numbersAndSymbolics.RemoveAt(Times);
                numbersAndSymbolics.RemoveAt(Times);
                numbersAndSymbolics.RemoveAt(Times);
            }
            while (CheckSymbolic('*', numbersAndSymbolics))
            {
                temporaryA = Convert.ToDouble(numbersAndSymbolics[Times - 2]) * Convert.ToDouble(numbersAndSymbolics[Times + 1]) - Convert.ToDouble(numbersAndSymbolics[Times - 1]) * Convert.ToDouble(numbersAndSymbolics[Times + 2]);
                temporaryB = Convert.ToDouble(numbersAndSymbolics[Times - 2]) * Convert.ToDouble(numbersAndSymbolics[Times + 2]) + Convert.ToDouble(numbersAndSymbolics[Times - 1]) * Convert.ToDouble(numbersAndSymbolics[Times + 1]);
                numbersAndSymbolics[Times - 2] = Convert.ToString(temporaryA);
                numbersAndSymbolics[Times - 1] = Convert.ToString(temporaryB);
                numbersAndSymbolics.RemoveAt(Times);
                numbersAndSymbolics.RemoveAt(Times);
                numbersAndSymbolics.RemoveAt(Times);
            }
            while (CheckSymbolic('-', numbersAndSymbolics))
            {
                numbersAndSymbolics[Times - 2] = Convert.ToString(Convert.ToDouble(numbersAndSymbolics[Times - 2]) - Convert.ToDouble(numbersAndSymbolics[Times + 1]));
                numbersAndSymbolics[Times - 1] = Convert.ToString(Convert.ToDouble(numbersAndSymbolics[Times - 1]) - Convert.ToDouble(numbersAndSymbolics[Times + 2]));
                numbersAndSymbolics.RemoveAt(Times);
                numbersAndSymbolics.RemoveAt(Times);
                numbersAndSymbolics.RemoveAt(Times);
            }
            while (CheckSymbolic('+', numbersAndSymbolics))
            {
                numbersAndSymbolics[Times - 2] = Convert.ToString(Convert.ToDouble(numbersAndSymbolics[Times - 2]) + Convert.ToDouble(numbersAndSymbolics[Times + 1]));
                numbersAndSymbolics[Times - 1] = Convert.ToString(Convert.ToDouble(numbersAndSymbolics[Times - 1]) + Convert.ToDouble(numbersAndSymbolics[Times + 2]));
                numbersAndSymbolics.RemoveAt(Times);
                numbersAndSymbolics.RemoveAt(Times);
                numbersAndSymbolics.RemoveAt(Times);
            }
        }
        public void CheckError(List<string>numbersAndSymbolics)
        {
            Trace.Assert(numbersAndSymbolics.Count == 2, "Invalid Synatx");
        }
    }
    public sealed class NormalCaculator:BaseCaculator
    {
        public void CheckError(List<string> numbersAndSymbolics)
        {
            Trace.Assert(numbersAndSymbolics.Count == 1, "Invalid Synatx");
        }
    }
    public sealed class QuadraticEquation:BaseCaculator
    {
        private string Simplification()
        {
            List<string> numbersAndSymbolics = new List<string>();
            foreach (string numberAndSymbolic in CollectNumbersAndSymbolics())
                numbersAndSymbolics.Add(numberAndSymbolic);
            Caculator(ref numbersAndSymbolics);
            return numbersAndSymbolics[0];
        }
        public void GetNumbers()
        {
            Console.WriteLine("ax^2+bx+c=d");
            SolveQuadraticEquation(Convert.ToDouble(Simplification()), Convert.ToDouble(Simplification()), Convert.ToDouble(Simplification()), Convert.ToDouble(Simplification()));    
        }
        private void SolveQuadraticEquation(double a,double b,double c,double d)
        {
            if ((b * b - 4 * a * (c - d)) >= 0)
                Console.WriteLine("x1={0} , x2={1}", (-b + Math.Sqrt(b * b - 4 * a * (c - d))) / (2 * a), (-b - Math.Sqrt(b * b - 4 * a * (c - d))) / (2 * a));
            else
                Console.WriteLine("no real root");
        }
    }
    public sealed class Equations:BaseCaculator
    {
        private string Simplification()
        {
            List<string> numbersAndSymbolics = new List<string>();
            foreach (string numberAndSymbolic in CollectNumbersAndSymbolics())
                numbersAndSymbolics.Add(numberAndSymbolic);
            Caculator(ref numbersAndSymbolics);
            return numbersAndSymbolics[0];
        }
        public void GetNumbers()
        {
            Console.WriteLine("ax+by=c dx+ey=f");
            SolveEquations(Convert.ToDouble(Simplification()), Convert.ToDouble(Simplification()), Convert.ToDouble(Simplification()), Convert.ToDouble(Simplification()), Convert.ToDouble(Simplification()), Convert.ToDouble(Simplification()));
        }
        private void SolveEquations(double a,double b,double c,double d,double e,double f)
        {
            double y = (f - c * d / a) / (e - b * d / a);
            Console.WriteLine("x={0} , y={1}", (c - b * y) / a, y);
        }
    }
    public sealed class FactorialProfession: BaseCaculator
    {
        public void Factorial(long n)
        {
            FileStream oldResult = new FileStream("Result.txt", FileMode.Create, FileAccess.Write);
            StreamWriter result = new StreamWriter(oldResult);
            int lim = 0;
            lim = Convert.ToInt32(Console.ReadLine());
            try
            {
                int[] f = new int[lim];
                int i, j, s, up;
                for (i = 2, f[0] = 1; i <= n; i++)
                {
                    for (j = up = 0; j < lim; j++)
                    {
                        s = f[j] * i + up;
                        f[j] = s % 10;
                        up = s / 10;
                    }
                }
                for (i = lim - 1; f[i] == 0; i--) ;
                for (; i >= 0; i--)
                    result.Write(f[i]);
            }
            catch
            {
                lim *= 10;
                int[] f = new int[lim];
                int i, j, s, up;
                for (i = 2, f[0] = 1; i <= n; i++)
                {
                    for (j = up = 0; j < lim; j++)
                    {
                        s = f[j] * i + up;
                        f[j] = s % 10;
                        up = s / 10;
                    }
                }
                for (i = lim - 1; f[i] == 0; i--) ;
                for (; i >= 0; i--)
                    result.Write(f[i]);
            }
            result.Close();
            Console.WriteLine("Calculation completed!");
            Process.Start("result.txt");
        }
    }
}
