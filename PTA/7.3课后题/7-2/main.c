#include<stdio.h>
#include<stdint.h>
char r[8];
uint8_t ind = 0;
void input()
{
	char t[3];
	while (1)
	{
		scanf("%c", &t[0]);
		if (t[0] == '\n')
		{
			r[ind] = t[1];
			++ind;
			r[ind] = t[2];
			++ind;
			break;
		}
		else
		{
			t[1] = t[0];
			scanf("%c", &t[2]);
		}

	}
}
int main(void)
{
	input();
	input();
	input();
	input();
	printf("%s", r);
	return 0;
}
