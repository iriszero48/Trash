#include <stdio.h>

int main(int argc, char* argv[])
{
	int m, t, s;
	scanf("%d %d %d", &m, &t, &s);
	double e = (double)s / t;
	printf("%d", e > m ? 0 : (int)(m - e));
}
