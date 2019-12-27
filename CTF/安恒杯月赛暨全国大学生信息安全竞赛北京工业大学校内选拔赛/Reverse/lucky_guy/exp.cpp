#include <cstring>
#include <cstdio>
#include <cstdlib>
int main()
{
	char s[100] = {0x69, 0x63, 0x75, 0x67, 0x60, 0x6f, 0x66, 0x7f, 0x00};
	char f1[100] = "GXY{do_not_";
	char f2[100] = {0};
	char v1;

	strcat(f2, (const char *)&s);

	while (true)
	{
		for (auto j = 0; j <= 7; ++j)
		{
			if (j % 2 == 1)
				v1 = *(f2 + j) - 2;
			else
				v1 = *(f2 + j) - 1;
			*(f2 + j) = v1;
		}
		puts("OK, it's flag:");
		memset(&s, 0, 0x28uLL);
		strcat((char *)&s, f1);
		strcat((char *)&s, f2);
		printf("%s", &s);
		system("pause");
	}
}
