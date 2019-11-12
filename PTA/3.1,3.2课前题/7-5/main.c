#include<stdio.h>
int main()
{
	struct
	{
		float a;
		int b;
		char c;
		float d;
	} f;
	scanf("%f %d %c %f", &f.a, &f.b, &f.c, &f.d);
	printf("%c %d %.2f %.2f", f.c, f.b, f.a, f.d);
	return 0;
}
