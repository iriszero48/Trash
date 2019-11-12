#include<stdio.h>
#include<stdint.h>
int  main(void)
{
	uint8_t a, b;
	char c;
	scanf("%d%d %c", &a, &b, &c);
	switch (b)
	{
	case 90:
		if (c == 'm')
		{
			printf("%.2f", a* 6.95*0.95);
		}
		else
		{
			printf("%.2f", a* 6.95*0.97);
		}
		break;
	case 93:
		if (c == 'm')
		{
			printf("%.2f", a* 7.44*0.95);
		}
		else
		{
			printf("%.2f", a* 7.44*0.97);
		}
		break;
	case 97:
		if (c == 'm')
		{
			printf("%.2f", a* 7.93*0.95);
		}
		else
		{
			printf("%.2f", a* 7.93*0.97);
		}
		break;
	}
	return 0;
}
