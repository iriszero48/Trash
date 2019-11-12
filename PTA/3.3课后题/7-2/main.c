#include<stdio.h>
#include<stdint.h>
#include<stdbool.h>
union mnd
{
	uint16_t y;
	uint8_t m;
	uint8_t d;
};
union mnd md;
int main(void)
{
	scanf("%d/", &md.y);
	if ((md.y % 4 == 0 && md.y % 100 != 0) || md.y % 400 == 0)
	{
		scanf("%d/", &md.m);
		switch (md.m)
		{
		case 1:
			scanf("%d", &md.d);
			printf("%d", md.d);
			break;
		case 2:
			scanf("%d", &md.d);
			printf("%d", 31 + md.d);
			break;
		case 3:
			scanf("%d", &md.d);
			printf("%d", 31 + 29 + md.d);
			break;
		case 4:
			scanf("%d", &md.d);
			printf("%d", 31 * 2 + 29 + md.d);
			break;
		case 5:
			scanf("%d", &md.d);
			printf("%d", 31 * 2 + 29 + 30 + md.d);
			break;
		case 6:
			scanf("%d", &md.d);
			printf("%d", 31 * 3 + 29 + 30 + md.d);
			break;
		case 7:
			scanf("%d", &md.d);
			printf("%d", 31 * 3 + 29 + 30 * 2 + md.d);
			break;
		case 8:
			scanf("%d", &md.d);
			printf("%d", 31 * 4 + 29 + 30 * 2 + md.d);
			break;
		case 9:
			scanf("%d", &md.d);
			printf("%d", 31 * 5 + 29 + 30 * 2 + md.d);
			break;
		case 10:
			scanf("%d", &md.d);
			printf("%d", 31 * 5 + 29 + 30 * 3 + md.d);
			break;
		case 11:
			scanf("%d", &md.d);
			printf("%d", 31 * 6 + 29 + 30 * 3 + md.d);
			break;
		case 12:
			scanf("%d", &md.d);
			printf("%d", 31 * 6 + 29 + 30 * 4 + md.d);
			break;
		}
	}
	else
	{
		scanf("%d/", &md.m);
		switch (md.m)
		{
		case 1:
			scanf("%d", &md.d);
			printf("%d", md.d);
			break;
		case 2:
			scanf("%d", &md.d);
			printf("%d", 31 + md.d);
			break;
		case 3:
			scanf("%d", &md.d);
			printf("%d", 31 + 28 + md.d);
			break;
		case 4:
			scanf("%d", &md.d);
			printf("%d", 31 * 2 + 28 + md.d);
			break;
		case 5:
			scanf("%d", &md.d);
			printf("%d", 31 * 2 + 28 + 30 + md.d);
			break;
		case 6:
			scanf("%d", &md.d);
			printf("%d", 31 * 3 + 28 + 30 + md.d);
			break;
		case 7:
			scanf("%d", &md.d);
			printf("%d", 31 * 3 + 28 + 30 * 2 + md.d);
			break;
		case 8:
			scanf("%d", &md.d);
			printf("%d", 31 * 4 + 28 + 30 * 2 + md.d);
			break;
		case 9:
			scanf("%d", &md.d);
			printf("%d", 31 * 5 + 28 + 30 * 2 + md.d);
			break;
		case 10:
			scanf("%d", &md.d);
			printf("%d", 31 * 5 + 28 + 30 * 3 + md.d);
			break;
		case 11:
			scanf("%d", &md.d);
			printf("%d", 31 * 6 + 28 + 30 * 3 + md.d);
			break;
		case 12:
			scanf("%d", &md.d);
			printf("%d", 31 * 6 + 28 + 30 * 4 + md.d);
			break;
		}
	}
	return 0;
}
