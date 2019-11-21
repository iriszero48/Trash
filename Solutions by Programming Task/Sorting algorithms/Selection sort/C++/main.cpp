#include <algorithm>

template <typename T>
void SelectionSort(T first, T last)
{
	for (auto i = first; i != last; ++i) std::iter_swap(i, std::min_element(i, last));
}
