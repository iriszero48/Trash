#include<stdio.h>
int main()
{
	int n,i;
	scanf("%d", &n);
	double sum = 0;
	for (i = 1; i <= n; i++)
	{
		sum = sum + 1.0 / (2 * i - 1);
	}
	printf("sum = %.6f", sum);
	return 0;
}
