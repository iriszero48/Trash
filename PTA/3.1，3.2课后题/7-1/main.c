#pragma warning (disable:4996)

#include<stdio.h>
#include<math.h>
int main()
{
	int x;
	double tax;
	scanf("%d", &x);
	if (x <= 1600)
	{
		tax = 0;
	}
	else if (x <= 2500)
	{
		tax = 0.05;
	}
	else if (x <= 3500)
	{
		tax = 0.1;
	}
	else if (x <= 4500)
	{
		tax = 0.15;
	}
	else if(x>4500)
	{
		tax = 0.2;
	}
	if (tax == 0)
	{
		printf("%.2f", (x - 1600));
	}
	else
	{
		printf("%.2f", tax*(x - 1600));
	}
	return 0;
}
