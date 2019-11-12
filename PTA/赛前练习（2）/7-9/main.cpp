#include <iostream>
#include<string>
#include<stack>
#include<vector>
using namespace std;

int main()
{
	auto line = 0;
	string input;
	cin >> line;
	getchar();
	getline(cin,input);
	vector<stack<char>>r(line);
	auto index = 0;
	for (auto& i : input)
	{
		r[index].push(i);
		++index;
		if (index == 4)
		{
			index = 0;
		}
	}
	int s = r[0].size();
	for (auto& i : r)
	{
		if (i.size() != s)
		{
			i.push(' ');
		}
		while (i.size())
		{
			cout << i.top();
			i.pop();
		}
		cout << endl;
	}
}
