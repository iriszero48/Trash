#include <stdio.h>
#include <stdint.h>
int main()
{
	struct 
	{
		uint16_t input;
		uint16_t five;
		uint16_t two;
		uint16_t one;
		uint16_t count;
	} all;
	all.count = 0;
	scanf("%d", &all.input);
	if (all.input > 8 && all.input < 100)
	{
		for (all.five = all.input; all.five >= 1; --all.five)
		{
			for (all.two = all.input; all.two >= 1; --all.two)
			{
				for (all.one = all.input; all.one >= 1; --all.one)
				{
					if (5 * all.five + 2 * all.two + all.one == all.input)
					{
						printf("fen5:%d, fen2:%d, fen1:%d, total:%d\n", all.five, all.two, all.one, all.five + all.two + all.one);
						++all.count;
					}
				}
			}
		}
	}
	printf("count = %d", all.count);
	return 0;
}
