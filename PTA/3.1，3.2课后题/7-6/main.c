#include<stdio.h>
#include<math.h>
int main(void) 
{
	int a, b, c;
	scanf("%d%d%d", &a, &b, &c);
	if ((!(a + b > c) || !(b + c > a) || !(a + c > b)))
	{
		printf("These sides do not correspond to a valid triangle");
	}
	else
	{
		double s = (a + b + c) / 2.0;
		printf("area = %.2f; perimeter = %.2f", sqrt(s*(s - a)*(s - b)*(s - c)), (double)a + b + c);
	}
	return 0;
}
