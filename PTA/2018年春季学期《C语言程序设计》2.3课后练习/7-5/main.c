#include <stdio.h>
#include <math.h>
int main() 
{
	double m, r, y;
	scanf("%lf%lf%lf", &m, &y, &r);
	printf("interest = %.2f", m*pow(1 + r, y) - m);
	return 0;
}
