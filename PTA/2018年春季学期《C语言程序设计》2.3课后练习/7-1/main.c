#include <stdio.h>
int main() 
{
	double x, cost;
	scanf("%lf", &x);
	if (x < 0)
	{
		printf("Invalid Value!");
	}
	else if(x <= 50)
	{
		printf("cost = %.2f", x*0.53);
	}
	else
	{
		printf("cost = %.2f", 50 * 0.53 + (x - 50)*(0.53 + 0.05));
	}
	return 0;
}
