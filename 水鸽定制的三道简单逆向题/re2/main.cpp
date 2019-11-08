#include <iostream>
#include <algorithm>
#include <list>

#define _CRT_SECURE_NO_WARNINGS
#pragma warning(disable : 4996)

using namespace std;

int main()
{
	const string flag = "666C61677B38303865306337363339393330313734666231633862313065373236346332617D";
	char input[0x100] = {0};
	cout << "flag:";
	cin >> input;
	cout << (flag == [](const string& str)
	{
#define ToHex(x) ((x) > 9 ? (x) + 55 : (x) + 48)
		auto s = str.c_str();
		auto res = new char[str.length() * 2];
		auto _res = res;
		const auto end = s + str.length();
		for (; s < end; ++s)
		{
			*res++ = ToHex(static_cast<uint8_t>(*s) >> 4);
			*res++ = ToHex(static_cast<uint8_t>(*s) % 16);
		}
		*res = 0;
		auto r = std::string(_res);
		delete[] _res;
		return r;
	}(input)
		         ? "true"
		         : "false");
}
