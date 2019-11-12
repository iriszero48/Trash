#include<stdio.h>
#include<stdint.h>
int main()
{
	uint16_t sum = 0;
	for (int16_t x;;)
	{
		scanf("%d", &x);
		if (x <= 0)
		{
			break;
		}
		if (x % 2 != 0)
		{
			sum += x;
		}
	}
	printf("%d", sum);
	return 0;
}
