#include <cstdlib>
#include <iostream>
#include <string>
#include <cmath>

using namespace std;
int main()
{
	int x;
	cin >> x;
	srand(x);
	for (int i = 0; i < x; ++i)
	{
		auto r = 1 + rand() % 35;
		printf("%s%3d%%\n", (string(r, '>') + string(35 - r, '.')).c_str(), static_cast<int>(round(r / 35. * 100)));
	}
}
