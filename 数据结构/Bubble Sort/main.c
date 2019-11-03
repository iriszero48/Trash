#include <stdio.h>
#include <stdbool.h>

void BubbleSort(int *a, int length)
{
	for (bool swapped = true; swapped; --length)
	{
		swapped = false;
		for (int i = 0; i != length - 1; ++i)
		{
			if (a[i] > a[i + 1])
			{
				int temp = a[i];
				a[i] = a[i + 1];
				a[i + 1] = temp;
				swapped = true;
			}
		}
	}
}

int main(int argc, char **argv)
{
	int a[5] = {0};
	for (int i = 0; i < 5; ++i)
	{
		scanf("%d", &a[i]);
	}
	BubbleSort(a, 5);
	for (int i = 0; i < 5; ++i)
	{
		printf("%d ", a[i]);
	}
}
