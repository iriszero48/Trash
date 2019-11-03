#pragma warning (disable:4996)
#include<stdio.h>
#include<stdlib.h>
#include<stdint.h>
#include<string.h>
typedef struct book
{
	char name[100];
	float price;
}book;
int compare(const void *a, const void *b)
{
	return (*(book *)a).price > (*(book *)b).price ? 1 : -1;
}
int main()
{
	int count;
	scanf("%d", &count);
	if (count > 0)
	{
		struct book *books = (struct book *)malloc(sizeof(struct book)*count);
		getchar();
		for (uint8_t i = 0; i < count; ++i)
		{
			fgets(books[i].name, 33, stdin);
			books[i].name[strlen(books[i].name) - 1] = '\0';
			scanf("%f", &books[i].price);
			getchar();
		}
		qsort(books, count, sizeof(books[0]), compare);
		if (count == 1)
		{
			printf("%.2f, %s\n%.2f, %s", books[0].price, books[0].name, books[0].price, books[0].name);
		}
		else
		{
			printf("%.2f, %s\n%.2f, %s", books[count - 1].price, books[count - 1].name, books[0].price, books[0].name);
		}
	}
	else
	{
		printf("0.00, \n0.00, \n");
	}
	return 0;
}
