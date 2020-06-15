#include <iostream>

int main(int argc, char* argv[])
{
	auto x = 0;
	std::cin >> x;
	std::cout << "Today, I ate " << x << " apple" << (x > 1 ? "s" : "") << ".";
}
