#include <iostream>

using namespace std;

uint64_t ids[1001] = { 0 };
uint16_t indexs[1001] = { 0 };

int main()
{
	int n;
	cin >> n;
	for (auto i = 0; i < n; ++i)
	{
		uint64_t id;
		uint16_t key, val;
		cin >> id >> key >> val;
		ids[key] = id;
		indexs[key] = val;
	}
	cin >> n;
	for (auto i = 0; i < n; ++i)
	{
		int m;
		cin >> m;
		cout << ids[m] << " " << indexs[m] << endl;
	}
}
