#include <iostream>  
#include <algorithm>  
#include <vector>  
#include <iomanip>
#include <string>
using namespace std;
class Book 
{
public:
	string name;
	float price;
	Book(bool red)
	{
		getline(cin, name);
		cin >> price;
		getchar();
	}
};
bool cmp(const Book &t1, const Book &t2)
{
	return t1.price > t2.price ? true : false;
}
int main(int argc, char *argv[])
{
	vector<Book> books;
	int count;
	cin >> count;
	if (count <= 0)
	{
		cout << "0.00, \n0.00 ";
		return 0;
	}
	getchar();
	for (int i = 0; i < count; ++i)
	{
		books.push_back(Book(true));
	}
	sort(books.begin(), books.end(), cmp);
	cout << setiosflags(ios::fixed) << setprecision(2) << books[0].price << ", " + books[0].name << endl << setiosflags(ios::fixed) << setprecision(2) << books[books.size() - 1].price << ", " + books[books.size() - 1].name;
	return 0;
}
