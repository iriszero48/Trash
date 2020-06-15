#include <iostream>

int main(int argc, char* argv[])
{
	auto y = 0;
	std::cin >> y;
	std::cout << (y % 4 == 0 && (y % 100 != 0 || y % 400 == 0));
}
