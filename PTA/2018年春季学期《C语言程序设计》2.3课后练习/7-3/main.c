#include <stdio.h>
int main() 
{
	double x,y;
	scanf("%lf", &x);
	if (x != 10)
	{
		y = x;
	}
	else
	{
		y = 1 / x;
	}
	printf("f(%.1f) = %.1f", x, y);
	return 0;
}
