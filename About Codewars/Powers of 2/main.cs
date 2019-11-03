using System.Numerics;
using System;
public class Kata
{
  public static BigInteger[] PowersOfTwo(int n)
  {
    BigInteger[] res = new BigInteger[n+1]; 
    for(int i = 0;i<=n;++i)
    {
      res[i]=BigInteger.Pow(new BigInteger(2),i);
    }
    return res;
  }
}
