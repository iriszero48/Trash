#include <iostream>
#include <vector>
#include <algorithm>
#include <numeric>
#include <string>
#include <cmath>

int main(int argc, char* argv[])
{
	const auto n = atoi(("1" + std::string(getchar() - 48, '0')).c_str());
	std::vector<int> v(n - 1);
	generate(v.begin(), v.end(), [n = 1]() mutable {return n++; });
	for_each(v.begin(), v.end(),[&](auto x)
	{
		auto y = std::to_string(x);
		if (std::accumulate(y.begin(), y.end(), 0, [](const int t, const char i) {return t + static_cast<int>(powf(i - 48, 5)); }) == x) std::cout << x << std::endl;
	});
}
