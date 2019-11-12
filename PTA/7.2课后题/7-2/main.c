#define _CRT_SECURE_NO_WARNINGS
#include<stdio.h>
#include<stdlib.h>
#include<stdint.h>
#include<stdbool.h>
uint8_t **maxt;
uint8_t col, i, j;
bool ex = false;
bool colu()
{
	for (uint8_t si = 0; si < col; ++si)
	{
		if (maxt[i][j] > maxt[si][j])
		{
			return true;
		}
	}
	return false;
}
bool row()
{
	for (uint8_t sj = 0; sj < col; ++sj)
	{
		if (maxt[i][j] < maxt[i][sj])
		{
			return true;
		}
	}
	return false;
}
int main(void)
{
	scanf("%d", &col);
	maxt = (uint8_t**)malloc(sizeof(uint8_t*)*col * col);
	for (i = 0; i < col; ++i)
	{
		*(maxt + i) = (uint8_t *)malloc(sizeof(uint8_t)*col);
		for (j = 0; j < col; ++j)
		{
			scanf("%d", (*(maxt + i) + j));
		}
	}
	for (i = 0; i < col; ++i)
	{
		for (j = 0; j < col; ++j)
		{
			if (row() || colu())
			{
				continue;
			}
			ex = true;
			printf("%d %d\n", i, j);
		}
	}
	if (!ex)
	{
		printf("NONE");
	}
	return 0;
}
