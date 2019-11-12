using static System.Console;
using System;
public class Program 
{
    public static void Main() 
    {
        int ts = Convert.ToInt32(ReadLine().Split(' ')[1]);
        string[] tns = ReadLine().Split(' ');
        int[] ns = new int[tns.Length];
        for(int ti=0;ti<tns.Length;++ti)
        {
            ns[ti]=Convert.ToInt32(tns[ti]);
        }
        for(int i =0;i<ts;++i)
        {
            string[] tas = ReadLine().Split(' ');
            int[] a = new int[tas.Length];
            for(int ti=0;ti<tas.Length;++ti)
            {
                a[ti]=Convert.ToInt32(tas[ti]);
            }
            if(a[0]==0)
            {
                ns[a[1]-1] = a[2];
            }
            else
            {
                int mi = 0;
                int max = 0;
                for(int ii = a[1]-1;ii<a[2];++ii)
                {
                    if(ns[ii]>max)
                    {
                        mi=ii+1;
                        max=ns[ii];
                    }
                }
                WriteLine($"{mi} {max}");
            }
        }
    }
}
