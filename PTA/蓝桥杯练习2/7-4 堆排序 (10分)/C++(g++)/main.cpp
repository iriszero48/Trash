#include <algorithm>
#include <iterator>
#include <iostream>
#include <vector>

int main()
{
	std::vector<int> data{};
	int n, x;
	std::cin >> n;
	for (int i = 0; i < n; ++i)
	{
		std::cin >> x;
		data.push_back(x);
	}
	std::make_heap(data.begin(), data.end());
	for (auto last = data.end(); last != data.begin(); --last)
	{
		std::copy(data.begin(), data.end(), std::ostream_iterator<int>(std::cout, " "));
		std::cout << "\n";
		std::pop_heap(data.begin(), last);
	}
}
