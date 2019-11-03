#include <iostream>

template<typename T>
class Node
{
public:
	T Value;
	Node* Left;
	Node* Right;
	
	Node(const T &v, Node* l = nullptr, Node* r = nullptr) :Value(v), Left(l), Right(r) {}
	void Preorder(void(*fun)(T)) const
	{
		fun(Value);
		if (Left) Left->Preorder(fun);
		if (Right) Right->Preorder(fun);
	}
	void Inorder(void(*fun)(T)) const
	{
		if (Left) Left->Inorder(fun);
		fun(Value);
		if (Right) Right->Inorder(fun);
	}
	void Postorder(void(*fun)(T)) const
	{
		if (Left) Left->Postorder(fun);
		if (Right) Right->Postorder(fun);
		fun(Value);
	}
};

int main()
{
	Node<int> n(1,
		new Node<int>(2,
			new Node<int>(4,
				new Node<int>(7)),
			new Node<int>(5)),
		new Node<int>(3,
			new Node<int>(6,
				new Node<int>(8),
				new Node<int>(9))));
	auto fun = [](auto n) { std::cout << n << " "; };
	n.Preorder(fun);
	std::cout << std::endl;
	n.Inorder(fun);
	std::cout << std::endl;
	n.Postorder(fun);
}
