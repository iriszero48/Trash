#include <algorithm>
#include <functional>

template <typename T>
void QuickSort(T first, T last)
{
	if (last - first < 2) return;
	auto split = std::partition(first + 1, last, std::bind2nd(std::less<typename std::iterator_traits<T>::value_type>(), *first));
	std::iter_swap(first, split - 1);
	QuickSort(first, split - 1);
	QuickSort(split, last);
}
