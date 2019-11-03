using namespace System;
using namespace System::Collections::Generic;

template<typename T>
public class Node
{
public:
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
private:
	T Value;
	Node* Left;
	Node* Right;
};

Node<int>* BuildIntTree(List<String ^> ^input , int index = 0)
{
	auto level = input[index]->Split(' ')->Length - 1;
	auto child = gcnew List<int>();
	for (auto i = index + 1; i < input->Count; ++i)
	{
		if (input[i]->Split(' ')->Length - 1 == level + 1) child->Add(i);
		if (input[i]->Split(' ')->Length - 1 < level + 1) break;
	}
	return new Node<int>(Convert::ToInt32(input[index]->Trim()), child->Count > 0 ? BuildIntTree(input, child[0]) : nullptr, child->Count > 1 ? BuildIntTree(input, child[1]) : nullptr);
}

int main()
{
	auto input = gcnew List<String ^>();
	for (auto i = Console::ReadLine(); i != ""; i = Console::ReadLine()) input->Add(i);
	auto n = BuildIntTree(input);
	auto fun = [](auto n) { Console::Write(n.ToString() + " "); };
	n->Preorder(fun);
	Console::WriteLine();
	n->Inorder(fun); 
	Console::WriteLine();
	n->Postorder(fun);
}
