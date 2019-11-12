#include<stdio.h>
int main()
{
	int n, y;
	scanf("%d", &n);
	if (n < 0)
	{
		y = -1;
	}
	else if(n==0)
	{
		y = 0;
	}
	else
	{
		y = 1;
	}
	printf("sign(%d) = %d", n, y);
	return 0;
}
