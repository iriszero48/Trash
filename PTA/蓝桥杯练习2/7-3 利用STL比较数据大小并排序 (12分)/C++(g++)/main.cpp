#include <iostream>
#include <queue>

int main()
{
	std::priority_queue<int, std::vector<int>, std::greater<>> data{};
	int n;
	while (std::cin >> n) data.push(n);
	std::cout << u8"从标准设备读入数据，直到输入是非整型数据为止\n";
	while (!data.empty())
	{
		std::cout << " " << data.top();
		data.pop();
	}
}
