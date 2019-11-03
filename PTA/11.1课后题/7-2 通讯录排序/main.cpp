#include <iostream>  
#include <algorithm>  
#include <vector>  
#include <string>
using namespace std;
class Friend
{
public:
	int brithday;
	Friend(string name,int brithday,string phone)
	{
		this->name = name;
		this->brithday = brithday;
		this->phone = phone;
	}
	string display()
	{
		return name + " " + Brithday() + " " + phone;
	}
private:
	string name;
	string phone;
	string Brithday()
	{
		return to_string(brithday);
	}
};
bool cmp(const Friend &t1, const Friend &t2)
{
	return t1.brithday < t2.brithday ? true : false;
}
int main()
{
	int count;
	cin >> count;
	vector<Friend> friends;
	for (int i = 0; i < count; ++i)
	{
		string name, phone;
		int brithday;
		cin >> name >> brithday >> phone;
		friends.push_back(Friend(name, brithday, phone));
	}
	sort(friends.begin(), friends.end(), cmp);
	for (int i = 0; i < count; ++i)
	{
		cout << friends[i].display() << endl;
	}
	return 0;
}
