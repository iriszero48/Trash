#include<stdio.h>
#include<math.h>
int main()
{
	int a, b;
	double s;
	scanf("%d%d", &a, &b);
	for ( s = 0; a <= b; a+=1)
	{
		s += pow(a, 2) + 1.0 / a;
	}
	printf("sum = %.6f", s);
	return 0;
}
