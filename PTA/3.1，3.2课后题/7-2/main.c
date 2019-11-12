#include<stdio.h>
int main()
{
	int data[3];
	for (int i = 0; i < 3; i++)
	{
		scanf("%d", &data[i]);
	}
	int i, j, mix, temp;
	for (i = 0; i<3 - 1; i++)
	{
		int mix = i;
		for (j = i + 1; j<3; j++)
		{
			if (data[j] < data[mix])
			{
				mix = j;
			}
		}
		if (i != mix)
		{
			temp = data[i];
			data[i] = data[mix];
			data[mix] = temp;
		}
	}
	printf("%d->%d->%d", data[0], data[1], data[2]);
	return 0;
}
