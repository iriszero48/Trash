#include<iostream>

template<typename T> 
struct SinglyLinkedListNode
{
	SinglyLinkedListNode* next;
	T data;
	SinglyLinkedListNode(T data, SinglyLinkedListNode* next = nullptr) : next(next), data(data) {}
};

template<typename T>
void Insert(SinglyLinkedListNode<T>* node, SinglyLinkedListNode<T>* newNode)
{
	newNode->next = node->next;
	node->next = newNode;
}

template<typename T>
void Delete(SinglyLinkedListNode<T>* node, SinglyLinkedListNode<T>* delNode)
{
	node->next = delNode;
	delete delNode;
}

int main()
{
	auto a = new SinglyLinkedListNode<int>(1);
	auto b = new SinglyLinkedListNode<int>(2);
	Insert(a, b);
	std::cout << a->data << " " << a->next << " " << a->next->data << " " << a->next->next;
}
