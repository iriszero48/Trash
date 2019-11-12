#include<stdio.h>
#include<stdint.h>

int main()
{
	uint16_t x = 0;
	for (; !(x % 5 == 1 && x % 6 == 5 && x % 7 == 4 && x % 11 == 10); ++x);
	printf("%d", x);
	return 0;
}
