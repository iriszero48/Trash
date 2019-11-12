#include<stdio.h>
#include<math.h>
#include<stdint.h>
double fact(double x);
int main()
{
	double result,x;
	result=fact(x);
	printf("result = %.0f",result);
}
double fact(double x)
{
	uint8_t N,i;
	double sum;
	scanf("%d",&N);
	for(i=1;i<=N;i++)
	{
		x=pow(2,i);
		sum+=x;
	}
	return sum;
}
