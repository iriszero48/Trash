using System.Linq;
public static class Kata
{
	public static int sumTwoSmallestNumbers(int[] numbers) => numbers.OrderBy(i => i).Take(2).Sum();
}
