#include<stdio.h>
int main(void) 
{
	int input;
	scanf("%d", &input);
	if (input > 2003 && input <= 2100)
	{
		for (int i = 2001; i <= input; i++)
		{
			if ((i % 4 == 0 && i % 100 != 0) || (i % 400 == 0))
			{
				printf("%d\n", i);
			}
		}
	}
	else if (input > 2000 && input < 2004)
	{
		printf("None");
	}
	else
	{
		printf("Invalid year!");
	}
	return 0;
}
