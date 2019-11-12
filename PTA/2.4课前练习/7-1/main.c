#include <stdio.h>
int main()
{
	double i,n,sum;
	scanf("%lf", &n);
	sum = 0;
	for (i = 1; i <= n; i++)
	{
		sum = sum + 1 / i;
	}
	printf("sum = %.6f", sum);
	return 0;
}
