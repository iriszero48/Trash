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
			if (*i > *(i + 1))
			{
				std::iter_swap(i, i + 1);
				swapped = true;
			}
		}
	}
}
