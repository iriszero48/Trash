#include <vector>
#include <algorithm>

std::vector<int> two_oldest_ages(std::vector<int> ages)
{
	std::sort(ages.begin(), ages.end(), std::greater<int>());
	return { ages[1], ages[0] };
}
