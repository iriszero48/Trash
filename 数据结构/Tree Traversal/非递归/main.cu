#include "cuda_runtime.h"
#include "device_launch_parameters.h"
#include <stdlib.h>
#include <stdio.h>
#include <iostream>

#define v 0
#define l 1
#define r 2

__host__ void Preorder(void*** nodes, void(*fun)(void*))
{
	int id = 0;
	printf("Preorder %d:", id);
	int* top = NULL;
	void*** stack = NULL;
	top = (int*)malloc(sizeof(int));
	stack = (void***)malloc(100 * sizeof(void**));
	*top = -1;
	stack[++*top] = nodes[id];
	while (*top > -1)
	{
		stack[99] = stack[(*top)--];
		fun(stack[99][v]);
		if (stack[99][r]) stack[++*top] = (void**)stack[99][r];
		if (stack[99][l]) stack[++*top] = (void**)stack[99][l];
	}
}

__host__ void Inorder(void*** nodes, void(*fun)(void*))
{
	int id = 0;
	printf("Preorder %d:", id);
	int* top = NULL;
	void*** stack = NULL;
	top = (int*)malloc(sizeof(int));
	stack = (void***)malloc(100 * sizeof(void***));
	*top = -1;
	for (stack[99] = nodes[id]; *top > -1 || stack[99]; stack[99] = (void**)stack[99][r])
	{
		while(stack[99])
		{
			stack[++*top] = stack[99];
			stack[99] = (void**)stack[99][l];
		}
		stack[99] = stack[(*top)--];
		fun(stack[99][v]);
	}
}

__host__ void Postorder(void*** nodes, void(*fun)(void*))
{
	int id = 0;
	printf("Preorder %d:", id);
	int* top = NULL;
	void*** stack = NULL;
	top = (int*)malloc(sizeof(int));
	stack = (void***)malloc(100 * sizeof(void***));
	*top = -1;
	stack[99] = nodes[id];
	while (*top > -1 || stack[99])
	{
		while (stack[99])
		{
			stack[++*top] = stack[99];
			stack[99] = (void**)stack[99][l];
		}
		if (stack[*top][r] && stack[*top][r] != (void*)stack[98]) stack[99] = (void**)stack[*top][r];
		else
		{
			fun(stack[*top][v]);
			stack[98] = stack[(*top)--];
		}
	}
}

__host__ void** Tree(void* value, void* left, void* right)
{
	void** node = NULL;
	node = (void**)malloc(3 * sizeof(void*));
	node[v] = value;
	node[l] = left;
	node[r] = right;
	return node;
}

__host__ void callCuda()
{
	int n[9];
	for (auto i = 0; i < 9; ++i)n[i] = i + 1;
	void*** nodes = NULL;
	nodes = (void***)malloc(5 * sizeof(void***));
	for (int i = 0; i < 5; i++)
	{
		nodes[i] = Tree((void*)&n[0],
			Tree((void*)&n[1],
				Tree((void*)&n[3],
					Tree((void*)&n[6], NULL, NULL),
					NULL),
				Tree((void*)&n[4], NULL, NULL)),
			Tree((void*)&n[2],
				Tree((void*)&n[5],
					Tree((void*)&n[7], NULL, NULL),
					Tree((void*)&n[8], NULL, NULL)),
				NULL));
	}
	auto fun = [](void* n) { std::cout << *(int*)n << " "; };
	Preorder(nodes, fun);
	std::cout << std::endl;
	Inorder(nodes, fun);
	std::cout << std::endl;
	Postorder(nodes, fun);
}

int main()
{
	callCuda();
    return 0;
}
