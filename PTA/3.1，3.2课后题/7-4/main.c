#include<stdio.h>
int main()
{
	int a, b;
	int exceed;
	scanf("%d %d", &a, &b);
	exceed = (double)(a - b) / b * 100 + 0.5;
	if (exceed<10) 
	{
		printf("OK\n");
	}
	else if (exceed<50) 
	{
		printf("Exceed %d%%. Ticket 200\n", exceed);
	}
	else
	{
		printf("Exceed %d%%. License Revoked\n", exceed);
	}
	return 0;
}
