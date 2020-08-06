using System.Linq;

public class Kata
{
  public static int FindEvenIndex(int[] arr) => Enumerable.Range(1, arr.Length).FirstOrDefault(i => arr.Take(i - 1).Sum() == arr.Skip(i).Sum()) - 1;
}
