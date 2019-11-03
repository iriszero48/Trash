#include <algorithm>
#include <iostream>
#include <iterator>

template <typename T>
void BubbleSort(T begin, T end) 
{
	auto swapped = true;
	while (begin != end-- && swapped) 
	{
		swapped = false;
		for (auto i = begin; i != end; ++i) 
		{
			if (*(i + 1) < *i) 
			{
				std::iter_swap(i, i + 1);
				swapped = true;
			}
		}
	}
}

int main() 
{
	int a[5] = { 0 };
	for (int i = 0; i < 5; ++i)
	{
		std::cin >> a[i];
	}
	BubbleSort(std::begin(a), std::end(a));
	copy(std::begin(a), std::end(a), std::ostream_iterator<int>(std::cout, " "));
	std::cout << "\n";
}
