#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
int main()
{
	uint16_t m;
	uint16_t n;
	bool isNone = 1;
	scanf("%d%d", &m, &n);
	for (uint16_t i = m; i <= n; ++i)
	{
		switch (i)
		{
		case 6:
			isNone = 0;
			printf("6 = 1 + 2 + 3\n");
			break;
		case 28:
			isNone = 0;
			printf("28 = 1 + 2 + 4 + 7 + 14\n");
			break;
		case 496:
			isNone = 0;
			printf("496 = 1 + 2 + 4 + 8 + 16 + 31 + 62 + 124 + 248\n");
			break;
		case 8128:
			isNone = 0;
			printf("8128 = 1 + 2 + 4 + 8 + 16 + 32 + 64 + 127 + 254 + 508 + 1016 + 2032 + 4064\n");
			break;
		}
	}
	if (isNone)
	{
		printf("None");
	}
	return 0;
}
