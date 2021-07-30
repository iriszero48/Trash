#pragma once

#include <string>
#include <charconv>
#include <optional>
#include <filesystem>

#ifndef _MSC_VER
#include <sstream>

template<typename T>
[[nodiscard]] std::optional<std::string> __To_SString_Impl__(const T& value) noexcept
{
	std::ostringstream ss;
	ss << value;
	return ss.str();
}
#endif

template<typename T, typename Str, typename Args>
[[nodiscard]] std::optional<T> __From_String_Impl__(const Str& value, const Args args) noexcept
{
	T res;
	const auto begin = value.data();
	const auto end = begin + value.length();
	auto [p, e] = std::from_chars(begin, end, res, args);
	if (e != std::errc{}) return {};
	return res;
}

template<typename T, typename...Args>
[[nodiscard]] std::optional<std::string> __To_String_Impl__(const T& value, Args&& ... args) noexcept
{
	char res[sizeof(T) * 8 + 1] = { 0 };
	auto [p, e] = std::to_chars(res, res + 65, value, std::forward<Args>(args)...);
	if (e != std::errc{}) return {};
	return res;
}

namespace Convert
{
    constexpr auto Version = "1.0.0";

	template<typename T, std::enable_if_t<std::is_integral_v<T>, int> = 0>
	[[nodiscard]] decltype(auto) ToString(const T value, const int base = 10) noexcept
	{
		return __To_String_Impl__<T>(value, base);
	}

#ifdef _MSC_VER
	template<typename T, std::enable_if_t<std::is_floating_point_v<T>, int> = 0>
	[[nodiscard]] decltype(auto) ToString(const T value) noexcept
	{
		return __To_String_Impl__<T>(value);
	}

	template<typename T, std::enable_if_t<std::is_floating_point_v<T>, int> = 0>
	[[nodiscard]] decltype(auto) ToString(const T value, const std::chars_format& fmt) noexcept
	{
		return __To_String_Impl__<T>(value, fmt);
	}

	template<typename T, std::enable_if_t<std::is_floating_point_v<T>, int> = 0>
	[[nodiscard]] decltype(auto) ToString(const T value, const std::chars_format& fmt, const int precision) noexcept
	{
		return __To_String_Impl__<T>(value, fmt, precision);
	}
#else
	template<typename T, std::enable_if_t<std::is_floating_point_v<T>, int> = 0>
	[[nodiscard]] decltype(auto) ToString(const T value) noexcept
	{
		return __To_SString_Impl__<T>(value);
	}
#endif

	template<typename T, typename Str, std::enable_if_t<std::is_integral_v<T>, int> = 0>
	[[nodiscard]] decltype(auto) FromString(const Str value, const int base = 10) noexcept
	{
		return __From_String_Impl__<T>(value, base);
	}

#ifdef _MSC_VER
	template<typename T, typename Str, std::enable_if_t<std::is_floating_point_v<T>, int> = 0>
	[[nodiscard]] decltype(auto) FromString(const Str value, const std::chars_format& fmt = std::chars_format::general) noexcept
	{
		return __From_String_Impl__<T>(value, fmt);
	}
#endif
}
