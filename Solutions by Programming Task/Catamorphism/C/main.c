#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

void* Redece(void* (*func)(void*, void*), void** lst, uint32_t size)
{
	void* res = *lst;
	for (uint32_t i = 1; i < size; i++) res = func(res, lst[i]);
	return res;
}

void* Fold(void* (*func)(void*, void*), void** lst, uint32_t size, void* init)
{
	void* res = init;
	for (uint32_t i = 0; i < size; i++) res = func(res, lst[i]);
	return res;
}

int Add(int a, int b)
{
	return a + b;
}

void main()
{
	int lst[] = { 1, 2, 3, 4, 5 };
	printf("%d\n", Redece(Add, lst, 5));
	printf("%d", Fold(Add, lst, 5, 1));
}
