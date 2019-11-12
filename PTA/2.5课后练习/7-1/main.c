#include<stdio.h>
double fact(int n)
{
	double sum = 1;
	for (int i = 1; i <= n; i++)
	{
		sum = sum*i;
	}
	return sum;
}
int main()
{
	int m, n;
	scanf("%d%d", &m, &n);
	printf("result = %.0f", fact(n) / (fact(m)*fact(n - m)));
	return 0;
}
