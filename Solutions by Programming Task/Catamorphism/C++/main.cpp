#include <numeric>
#include <vector>
#include <iostream>

template<typename T, typename... Args>
T LeftFold(T init, Args&&... args)
{
	return (init + ... + args);
}

template<typename T, typename... Args>
T RightFold(T init, Args&&... args)
{
	return (args + ... + init);
}

template<typename... Args>
auto LeftFoldWithoutInitialValue(Args&&... args)
{
	return (... + args);
}

template<typename... Args>
auto RightFoldWithoutInitialValue(Args&&... args)
{
	return (args + ...);
}

int main()
{
	std::vector<int> lst = { 1,2,3,4,5 };
	std::cout << std::accumulate(lst.begin(), lst.end(), 0) << std::endl;
	std::cout << std::accumulate(lst.begin(), lst.end(), 0, std::plus<int>()) << std::endl;
	std::cout << std::accumulate(lst.begin(), lst.end(), 0, [](auto a, auto b) { return a + b; }) << std::endl;
	std::cout << std::accumulate(lst.rbegin(), lst.rend(), 0, std::plus<int>()) << std::endl;
	std::cout << LeftFold(0, 1, 2, 3, 4, 5) << std::endl;
	std::cout << RightFold(0, 1, 2, 3, 4, 5) << std::endl;
	std::cout << LeftFoldWithoutInitialValue(0, 1, 2, 3, 4, 5) << std::endl;
	std::cout << RightFoldWithoutInitialValue(0, 1, 2, 3, 4, 5) << std::endl;
}
