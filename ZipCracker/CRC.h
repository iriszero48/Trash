#pragma once

#include <cstdint>
#include <array>
#include <algorithm>
#include <numeric>

namespace Crc
{
	template<typename It>
	std::uint32_t Crc32(It begin, It end)
	{
		static const auto table = []()
		{
			std::array<std::uint32_t, 256> tab{};
			std::generate(tab.begin(), tab.end(), [&, n = 0]() mutable 
			{
				constexpr std::uint32_t polynomial = 0xEDB88320uL;
				std::uint32_t checksum = n++;
				for (auto i = 0; i < 8; ++i)
					checksum = (checksum >> 1) ^ ((checksum & 0x1u) ? polynomial : 0);
				return checksum;
			});
			return tab;
		}();
		return ~std::accumulate(
			begin, end,
			~std::uint32_t{ 0 }, 
			[](const std::uint32_t checksum, const std::uint8_t value)
			{ return table.at(static_cast<std::uint8_t>(checksum ^ value)) ^ checksum >> 8u; });
	}
}
