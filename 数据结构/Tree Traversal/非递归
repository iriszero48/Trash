#include <iostream>
#include <stack>

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
		std::stack<const Node*> s;
		s.push(this);
		while (!s.empty())
		{
			const Node* n = s.top();
			s.pop();
			fun(n->Value);
			if (n->Right) s.push(n->Right);
			if (n->Left) s.push(n->Left);
		}
	}
	void Inorder(void(*fun)(T)) const
	{
		std::stack<const Node*> s;
		for (const Node* n = this; !s.empty() || n; n = n->Right)
		{
			while (n)
			{ 
				s.push(n);
				n = n->Left;
			}
			n = s.top();
			s.pop();
			fun(n->Value);
		}
	}
	void Postorder(void(*fun)(T)) const
	{
		std::stack<const Node*> s;
		const Node* n = this, *last = nullptr, *top = nullptr;
		while (!s.empty() || n)
		{
			while (n)
			{
				s.push(n);
				n = n->Left;
			}
			top = s.top();
			if (top->Right && top->Right != last) n = top->Right;
			else
			{
				fun(top->Value);
				last = s.top();
				s.pop();
			}
			
		}
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
	std::cout << std::endl;
}
