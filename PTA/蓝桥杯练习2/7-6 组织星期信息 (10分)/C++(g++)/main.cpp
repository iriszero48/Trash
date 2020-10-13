#include <iostream>
#include <unordered_map>

int main()
{
	std::unordered_map<std::string, int> data
	{
		{"Sunday", 1},
		{"Monday", 2},
		{"Tuesday", 3},
		{"Wednesday", 4},
		{"Thursday", 5},
		{"Friday", 6},
		{"Saturday", 7}
	};
	int c;
	std::cin >> c;
	for (auto i = 0; i < c; ++i)
	{
		std::string s;
		std::cin >> s;
		auto t = data.find(s);
		std::cout << (t != data.end() ? t->second : -1) << "\n";
	}
}
