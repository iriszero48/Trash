#include<stdio.h>
int main()
{
	int l = 0, b = 0, d = 0, o = 0;
	for (int i = 0; i < 10; i++)
	{
		char c = getchar();
		if (c >= 'a' && c <= 'z' || c >= 'A'&&c <= 'Z')
		{
			++l;
		}
		else if(c>='0' && c<='9')
		{
			++d;
		}
		else if(c==' '|| c=='\n')
		{
			++b;
		}
		else
		{
			++o;
		}
	}
	printf("letter = %d, blank = %d, digit = %d, other = %d", l, b, d, o);
	return 0;
}
