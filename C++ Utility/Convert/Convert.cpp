#include <charconv>
#include <sstream>

#include "Convert.h"

template <typename ...Args>
std::string __Arguments_Combine__(Args&&... args)
{
	std::ostringstream ss{};
	(ss << ... << args);
	return ss.str();
}

#define __Convert_ToStringFunc__(x) #x
#define __Convert_ToString__(x) __Convert_ToStringFunc__(x)
#define __Convert_Line__ __Convert_ToString__(__LINE__)

#define __Convert_ThrowEx__(...) throw std::runtime_error(__Arguments_Combine__( __FILE__ ": " __Convert_Line__ ": " __FUNCTION__ ": ", __VA_ARGS__))


namespace Convert
{
	int ToInt(const std::string& value, const int base)
	{
		int res;
		auto [p, e] = std::from_chars(value.data(), value.data() + value.length(), res, base);
		if (e != std::errc{}) __Convert_ThrowEx__("convert error: invalid literal: ", p);
		return res;
	}
}

#undef __Convert_ToStringFunc__
#undef __Convert_ToString__
#undef __Convert_Line__
#undef __Convert_ThrowEx__
