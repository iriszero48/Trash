#include <stdio.h>
#include <stdint.h>
int main()
{
	uint8_t input;
	scanf("%d", &input);
	struct 
	{
		uint8_t x : 4;
		uint8_t y : 4;
	} mu;
	for (mu.y = 1; mu.y <= input; ++mu.y)
	{
		for (mu.x = 1;  mu.x<= mu.y; ++mu.x)
		{
			printf("%d*%d=%-4d", mu.x, mu.y, mu.x*mu.y);
		}
		printf("\n");
	}
	return 0;
}
