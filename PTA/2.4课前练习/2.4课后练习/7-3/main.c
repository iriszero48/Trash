#include <stdio.h>
int main()
{
	int i, n, k = 1;
	double sum = 0, sum1;
	scanf("%d", &n);
	for (i = 1; i <= n; i++)
	{
		sum1 = 1.0 / (i * 3 - 2);
		sum = sum + sum1*k;
		k = -k;
	}
	printf("sum = %.3f", sum);
	return 0;
}
