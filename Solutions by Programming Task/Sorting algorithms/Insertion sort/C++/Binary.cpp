#include <algorithm>
template<typename T>
void BinaryInsertionSort(T begin, T end)
{
	for (auto i = begin; i != end; ++i) std::rotate(std::upper_bound(begin, i, *i), i, i + 1);
}
