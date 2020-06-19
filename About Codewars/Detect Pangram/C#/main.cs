using System.Linq;
using System.Text.RegularExpressions;

public static class Kata
{
  public static bool IsPangram(string str) => new Regex("[^a-z]").Replace(str.ToLower(), "").GroupBy(x => x).Count() == 26;
}
