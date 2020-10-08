#pragma once

#include <string>

namespace Convert
{
	[[nodiscard]] int ToInt(const std::string& value, int base = 10);
}
