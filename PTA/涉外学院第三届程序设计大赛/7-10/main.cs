using static System.Console;
using System;
public class Program 
{
    public static int[] s2i(string[] s)
    {
        int[] a = new int[s.Length];
        for(int i = 0;i<s.Length;++i)
        {
            a[i]=Convert.ToInt32(s[i]);
        }
        return a;
    }
    public static void Main() 
    {
        int t = Convert.ToInt32(ReadLine());
        for(int _i = 0;_i<t;++_i)
        {
            int count=0;
            ReadLine();
            int[] ms = s2i(ReadLine().Split(' '));
            for(int i=0;i<=ms.Length;++i)
            {
                for(int j = 0;j<ms.Length;++j)
                {
                    if(i<j && ms[i]>ms[j]) ++count;
                }
            }
            WriteLine(count);
        }
    }
}
