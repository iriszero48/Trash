using CaculatorVersionIDatabase;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
namespace CaculatorVersionI
{
    class CaculatorVersionI1
    {
        static void Main(string[] args)
        {
            byte select=1;
            Console.WriteLine("Now Loading...");
            NormalCaculator normalCaculator = new NormalCaculator();
            Statistics statistics = new Statistics();
            List<string> numbersAndSymbolics = new List<string>();
            List<double> numbers = new List<double>();
            Complex complex = new Complex();
            QuadraticEquation quadraticEquation = new QuadraticEquation();
            Equations equations = new Equations();
            FactorialProfession factorialProfession = new FactorialProfession();
            #region 1stVersionPart5
            /// <summary>
            /// 1st version result:false
            /// Console.WriteLine("Caculator Ver.I:1 20160125");
            /// </summary>
            /// <returns></returns>
            #endregion
            #region 2ndVersionPart2
            /// <summary>
            /// 2nd version complex caculator result:false
            /// Console.WriteLine("Caculator Ver.I:2 20160125");
            /// </summary>
            /// <returns></returns>
            #endregion
            #region 3rdVersionPart2
            /// <summary>
            /// using Ling
            /// Console.WriteLine("Caculator Ver.I:3 20160126");
            /// </summary>
            /// <returns></returns>
            #endregion
            #region 4thVersionPart1
            /// <summary>
            /// QuadraticEquation and Equations
            /// Console.WriteLine("Caculator Ver.I:4 20160126");
            /// </summary>
            /// <returns></returns>
            #endregion
            #region 5thVersionPart1
            /// <summary>
            /// QuadraticEquation and Equations
            /// Console.WriteLine("Caculator Ver.I:5 20160201");
            /// </summary>
            /// <returns></returns>
            #endregion
            Console.WriteLine("Caculator Ver.I:7 20160221");
            while (select!=7)
            {
                Console.WriteLine("1-Normal 2-Statistics 3-Complex 4-QuadraticEquation 5-Equations 6-FactorialProfession 7-Exit");
                select = Convert.ToByte(Console.ReadLine());
                Trace.Assert(select == 1 || select == 2 || select == 3 || select == 4 || select == 5 || select == 6 || select == 7, "Invalid Syntax");
                switch(select)
                {
                    case 1:
                        foreach (string numberAndSymbolic in normalCaculator.CollectNumbersAndSymbolics())
                            numbersAndSymbolics.Add(numberAndSymbolic);
                        normalCaculator.Caculator(ref numbersAndSymbolics);
                        normalCaculator.CheckError(numbersAndSymbolics);
                        Console.WriteLine(numbersAndSymbolics[0]);
                        numbersAndSymbolics.Clear();
                        break;
                    case 2:
                        statistics.StatisticsStart(ref numbers);
                        var result = numbers.Select(number => number);
                        #region 6thVersionPart1
                        /// <summary>
                        /// use system.io and linq
                        /// Console.WriteLine("Number Of Data Points:{0}\nTotal:{1}\nMean:{2}", usingNumbers.Count(), usingNumbers.Sum(), usingNumbers.Sum());
                        /// </summary>
                        /// <returns></returns>
                        #endregion
                        Console.WriteLine("Number Of Data Points:{0}\nTotal:{1}\nMean:{2}\nMaximum:{3}\nMinimum{4}\nMedian:{5}", result.Count(), result.Sum(), result.Average(), result.Max(), result.Min(), statistics.Median(numbers));
                        numbers.Clear();
                        break;
                    case 3:
                        foreach (string numberAndSymbolic in normalCaculator.CollectNumbersAndSymbolics())
                            numbersAndSymbolics.Add(numberAndSymbolic);
                        complex.Caculator(ref numbersAndSymbolics);
                        complex.CheckError(numbersAndSymbolics);
                        Console.WriteLine("{0},{1}",numbersAndSymbolics[0],numbersAndSymbolics[1]);
                        numbersAndSymbolics.Clear();
                        break;
                    case 4:
                        quadraticEquation.GetNumbers();
                        break;
                    case 5:
                        equations.GetNumbers();
                        break;
                    case 6:
                        foreach (string numberAndSymbolic in normalCaculator.CollectNumbersAndSymbolics())
                            numbersAndSymbolics.Add(numberAndSymbolic);
                        normalCaculator.Caculator(ref numbersAndSymbolics);
                        normalCaculator.CheckError(numbersAndSymbolics);
                        Console.WriteLine(numbersAndSymbolics[0]);
                        Trace.Assert(Convert.ToDouble(numbersAndSymbolics[0]) % 1 == 0, "Invalid Syntax");
                        factorialProfession.Factorial(Convert.ToInt64(numbersAndSymbolics[0]));
                        numbersAndSymbolics.Clear();
                        break;
                    case 7:
                        break;
                }
            }
        }
    }
}
