#define _CRT_SECURE_NO_WARNINGS
#include<stdio.h>
#include<stdlib.h>
#include<stdint.h>
#include<stdbool.h>
uint8_t *row;
uint8_t**maxt;
struct
{
	uint8_t number : 4;
	uint8_t col : 4;
	uint8_t i : 4;
} all;
uint8_t col;
uint8_t inst()
{
	uint8_t input;
	scanf("%d", &input);
	return input;
}
bool display()
{
	maxt =(uint8_t**) malloc(sizeof(uint8_t*)*col * col);
	for (uint8_t i = 0; i < col; ++i)
	{
		maxt[i] = (uint8_t *)malloc(sizeof(uint8_t)*col);
		for (uint8_t j = 0; j < col; ++j)
		{
			scanf("%d", &maxt[i][j]);
		}
	}
	for (uint8_t i = 1; i < col; ++i)
	{
		if(maxt[i][i-1]!=0)
		{
			return true;
		}
	}
	return false;
}
int main(void)
{
	all.number = inst();
	for (all.i = 0; all.i < all.number; ++all.i)
	{
		col = inst();
		if (display())
		{
			printf("NO\n");
		}
		else
		{
			printf("YES\n");
		}

	}
	return 0;
}
