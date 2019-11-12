#include <stdio.h>
int main() 
{
	int a, b;
	scanf("%d%d", &a, &b);
	int c = b - a;
	if (b % 100 < a % 100)
	{
		printf("%02d:%02d", c / 100, c % 100 - 40);
	}
	else
	{
		printf("%02d:%02d", c / 100, c % 100);
	}
	return 0;
}
