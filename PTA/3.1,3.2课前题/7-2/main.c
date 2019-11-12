#include<stdio.h>
int main()
{
	int input, n, count = 0;
	double sum = 0;
	scanf("%d", &n);
	for (int i = 0; i < n; i++)
	{
		scanf("%d", &input);
		sum += input;
		if (input >= 60)
		{
			count++;
		}
	}
	if (n == 0)
		n = 1;
	printf("average = %.1f\ncount = %d", sum / n, count);
	return 0;
}
