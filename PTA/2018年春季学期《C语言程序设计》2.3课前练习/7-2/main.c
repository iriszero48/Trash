#include <stdio.h>
int main() 
{
	double x = 0;
	scanf("%lf", &x);
	double f = 1 / x;
	if (x == 0)
		printf("f(0.0) = 0.0");
	else
		printf("f(%.1f) = %.1f", x, f);
	return 0;
}
