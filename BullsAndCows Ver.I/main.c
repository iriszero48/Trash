#pragma warning (disable:4996)
#include<stdio.h>
#include<stdlib.h>
#include<time.h>
int guess[4];
int random(int a, int b)
{
	return (rand() % (b - a + 1) + a);
}
int repeat()
{
	for (int i = 0; i < 4; ++i)
	{
		for (int j = 0; j < 4; ++j)
		{
			if (guess[i] == guess[j] && i != j)
			{
				return 1;
			}
		}
	}
	return 0;
}
void init()
{
	do
	{
		srand((unsigned)time(NULL));
		guess[0] = random(1, 9);
		for (int i = 1; i < 4; ++i)
		{
			guess[i] = random(0, 9);
		}
	} while (repeat());
}
int judge()
{
	int m = 0, n = 0;
	int input[4] = { getchar() - 48,getchar() - 48,getchar() - 48,getchar() - 48 };
	getchar();
	for (int i = 0; i < 4; ++i)
	{
		if (input[i] == guess[i])
		{
			++m;
		}
		for (int j = 0; j < 4; ++j)
		{
			if (input[i] == guess[j] && j != i)
			{
				++n;
			}
		}
	}
	if (m == 4)
	{
		return 1;
	}
	printf("%dA%dB\n", m, n);
}
int main()
{
	init();
	printf("guess 4bit\n");
	for (int i = 0; i < 15; ++i)
	{
		if (i == 7)
		{
			printf("7,cont?y/n");
			if (getchar() == 'n')
			{
				return 0;
			}
			getchar();
		}
		if (judge() == 1)
		{
			printf("fuck");
		}
	}
	printf("you died");
	return 0;
}
