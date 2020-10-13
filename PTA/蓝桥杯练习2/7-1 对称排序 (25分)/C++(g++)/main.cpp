#include <iostream>
#include <vector>
#include <iterator>

int main()
{
	int n, index = 1;
	while (true)
	{
		std::cin >> n;
		std::vector<std::string> data{};
		if (n==0)
		{
			exit(0);
		}
		for (auto i = 0; i < n; ++i)
		{
			std::string s;
			std::cin >> s;
			data.push_back(s);
		}
		std::cout << "SET " << index++ << "\n";
		for (auto i = 0; i < n; i+=2)
		{
			std::cout << data[i] << "\n";
		}
		for (auto i = ((n & 1) == 0 ? n - 1 : n - 2); i > 0; i -= 2)
		{
			std::cout << data[i] << "\n";
		}
	}
}
