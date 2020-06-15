#include <string>
#include <regex>
#include <sstream>

std::string to_camel_case(std::string text)
{
	std::ostringstream oss;
	const std::regex re("[_-]+");
	std::for_each(std::sregex_token_iterator(text.begin(), text.end(), re, -1),
	              std::sregex_token_iterator(),
	              [&, index = 0](auto& x) mutable
	              {
		              oss << (index++ == 0
			                      ? (x.str())
			                      : (std::string(1, std::toupper(x.str()[0])) + x.str().substr(1)));
	              });
	return oss.str();
}
