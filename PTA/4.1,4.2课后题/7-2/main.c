#include<stdio.h>
#include<stdint.h>

int main()
{
	for (int16_t i = 0,sum=0;; ++i)
	{
		char in;
		scanf("%c", &in);
		if (!(in >= '0' && in <= '9'))
		{
			printf("%d %d", i,sum);
			break;
		}
		sum += in - '0';
	}
	return 0;
}
