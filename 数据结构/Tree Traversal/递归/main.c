#include <stdio.h>
#include <stdlib.h>
#define T int

typedef struct node
{
	T value;
	struct node* left;
	struct node* right;
} node;

node* Tree(T v, node* l, node* r)
{
	node* n = (node*)malloc(sizeof(node));
	n->value = v;
	n->left = l;
	n->right = r;
	return n;
}

void Preorder(node* n, void(*fun)(T))
{
	fun(n->value);
	if (n->left) Preorder(n->left, fun);
	if (n->right) Preorder(n->right, fun);
}

void Inorder(node* n, void(*fun)(T))
{
	if (n->left) Inorder(n->left, fun);
	fun(n->value);
	if (n->right) Inorder(n->right, fun);
}

void Postorder(node* n, void(*fun)(T))
{
	if (n->left) Postorder(n->left, fun);
	if (n->right) Postorder(n->right, fun);
	fun(n->value);
}

void PrintInt(int n)
{
	printf("%d ", n);
}

int main()
{
	/*
			   1
		      / \
			 /   \
			2     3
		   / \   /
		  4   5 6
		 /     / \
		7     8   9
	*/
	node* n = Tree(1,
		Tree(2,
			Tree(4,
				Tree(7, NULL, NULL),
				NULL),
			Tree(5, NULL, NULL)),
		Tree(3,
			Tree(6,
				Tree(8, NULL, NULL),
				Tree(9, NULL, NULL)),
			NULL));
	Preorder(n, PrintInt);
	printf("\n");
	Inorder(n, PrintInt);
	printf("\n");
	Postorder(n, PrintInt);
}
