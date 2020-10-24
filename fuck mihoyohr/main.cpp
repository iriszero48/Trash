#include <map>
#include <iostream>
#include <algorithm>

std::map<char, char> Mmp
{
	{'z','B'},
	{'x','i'},
	{'e','g'},
	{'m','W'},
	{'f','e'},
	{'k','/'},
	{'p','.'},
	{'q','j'},
	{'c','p'},
	{'t','n'},
	{'s','_'}
};

int main(int argc, char* argv[])
{
	std::string str = "xzmexfskexxfxxcqeqcmxfeepftce";
	std::transform(str.begin(), str.end(), str.begin(), [&](auto x) { return Mmp[x]; });
	for (auto i = 0; i + 1 < str.length(); i+=2)
	{
		std::swap(str[i], str[i + 1]);
	}
	std::cout << str;
}
