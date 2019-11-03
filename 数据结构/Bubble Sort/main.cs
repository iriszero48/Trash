using System;
using System.Collections.Generic;
using static System.Console;

namespace BubbleSort
{
    public static class BubbleSortMethods
    {
        public static void BubbleSort<T>(this List<T> list) where T : IComparable
        {
            var count = list.Count - 1;
            for (var swapped = true; swapped; --count)  
            {
                swapped = false;
                for (var i = 0; i < count; ++i)
                {
                    if (list[i].CompareTo(list[i + 1]) > 0)
                    {
                        T temp = list[i + 1];
                        list[i + 1] = list[i];
                        list[i] = temp;
                        swapped = true;
                    }
                }
            }
        }
    }

    class Program
    {
        static void Main()
        {
            var list = new List<int>();
            foreach(var i in ReadLine().Split(' '))
            {
                list.Add(Convert.ToInt32(i));
            }
            list.BubbleSort();
            foreach(var i in list)
            {
                Write(i + " ");
            }
        }
    }
}
