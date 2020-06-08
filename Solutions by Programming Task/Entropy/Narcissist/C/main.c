#include <stdio.h>
#include <math.h>

int main()
{
	char count[256] = { 0 };
	FILE* fp = fopen("main.c", "r");
	int len;
	for (len = 0; !feof(fp); ++len)
	{
		count[fgetc(fp)]++;
	}
	fclose(fp);
	double entropy = 0;
	for (int i = 0; i < 256; i++)
	{
		if (count[i])
		{
			double freq = (double)count[i] / len;
			entropy -= freq * log(freq) / log(2);
		}
	}
	printf("%lf\n", entropy);
}
