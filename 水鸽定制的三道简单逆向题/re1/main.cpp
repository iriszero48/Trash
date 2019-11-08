#include <iostream>
#include <algorithm>
#include <list>
using namespace std;

static const string flag = "flag{d2fbe6e547afe2f46a1723e9fb4c443d}";

int main(int argc, char* argv[])
{
	std::list<char> res{};
	char input[0x100] = { 0 };
	cout << "flag:";
	cin >> input;
	std::copy_if(flag.begin(),flag.end(),std::back_inserter(res),[&, i = 0](auto x) mutable
	{
		return x == input[i++];
	});
	cout << (res.size() == flag.size() ? "true" : "false");
}
