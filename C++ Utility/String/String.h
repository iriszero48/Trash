#pragma once

#include <string>
#include <algorithm>
#include <sstream>
#include <charconv>
#include <filesystem>
#include <vector>

#define NewStringImpl(func, funcName, type)\
	template<typename...Args>\
    type funcName(Args&&...args)\
	{\
		type buf{};\
    	func(buf, std::forward<Args>(args)...);\
		return buf;\
	}

#define NewString(func, funcName)\
	NewStringImpl(func, funcName##Str, std::string)\
    NewStringImpl(func, funcName##Wstr, std::wstring)\
	NewStringImpl(func, funcName##U16str, std::u16string)\
	NewStringImpl(func, funcName##U32str, std::u32string)

#define __U8stringUseChar std::is_same_v<decltype(std::filesystem::path{}.u8string())::value_type, char>

namespace String
{
    constexpr auto Version = "1.0.0";

	namespace __Detail
	{
		template<class, class = void> struct HasValueType : std::false_type {};
		template<class T> struct HasValueType<T, std::void_t<typename T::value_type>> : std::true_type {};

		template<class T, typename toMatch>
		struct IsCString
		: std::integral_constant<
			bool,
			std::is_same<toMatch const *, typename std::decay<T>::type>::value ||
			std::is_same<toMatch *, typename std::decay<T>::type>::value
		> {};

		template<typename T>
		std::filesystem::path ToStringImpl(const T& t)
		{
			if constexpr (std::is_same_v<T, std::string::value_type>
				|| std::is_same_v<T, std::wstring::value_type>
				|| std::is_same_v<T, decltype(std::filesystem::path{}.u8string())::value_type>
				|| std::is_same_v<T, std::u16string::value_type>
				|| std::is_same_v<T, std::u32string::value_type>)
			{
				return std::basic_string<T>(1, t);
			}
			else if constexpr (std::is_integral_v<T>)
			{
				constexpr auto bufSiz = 65;
				char buf[bufSiz]{0};
				const auto [p, e] = std::to_chars(buf, buf + bufSiz, t);
				if (e != std::errc{}) throw std::runtime_error("ToStringImpl error: invalid literal: " + std::string(p));
				return buf;
			}
			else if constexpr (std::is_floating_point_v<T>)
			{
				std::ostringstream buf;
				buf << t;
				return buf.str();
			}
			else if constexpr (IsCString<T, char>::value
				|| IsCString<T, wchar_t>::value
				|| IsCString<T, decltype(std::filesystem::path{}.u8string())::value_type>::value
				|| IsCString<T, char16_t>::value
				|| IsCString<T, char32_t>::value)
			{
				return t;
			}
			else if constexpr (HasValueType<T>::value)
			{
				if constexpr ((std::is_base_of_v<std::basic_string<typename T::value_type>, T>
					|| std::is_base_of_v<std::basic_string_view<typename T::value_type>, T>))
				{
					return t;
				}
				else
				{
					std::u32string str = U"{";
					auto i = t.begin();
					auto end = t.end();
					if (end - i == 0) return U"{}";
					for (; i < end - 1; ++i)
					{
						str.append(ToStringImpl(*i).u32string());
						str.append(U", ");
					}
					str.append(ToStringImpl(*i).u32string());
					str.append(U"}");
					return str;
				}
			}
			else
			{
				std::ostringstream buf;
				buf << t;
				return buf.str();
			}
		}

		template<typename T> struct GetStrFunc{};
		template<> struct GetStrFunc<std::string> { decltype(auto) operator()(const std::filesystem::path& str) { return str.string(); } };
		template<> struct GetStrFunc<std::wstring> { decltype(auto) operator()(const std::filesystem::path& str) { return str.wstring(); } };
#ifndef __U8stringUseChar
		template<> struct GetStrFunc<std::u8string> { decltype(auto) operator()(const std::filesystem::path& str) { return str.u8string(); } };
#endif
		template<> struct GetStrFunc<std::u16string> { decltype(auto) operator()(const std::filesystem::path& str) { return str.u16string(); } };
		template<> struct GetStrFunc<std::u32string> { decltype(auto) operator()(const std::filesystem::path& str) { return str.u32string(); } };
	}

	template<typename Str>
	void ToUpper(Str& string)
	{
		std::transform(string.begin(), string.end(), string.begin(), static_cast<int(*)(int)>(std::toupper));
	}

	NewString(ToUpper, ToUpper)

	template<typename Str>
	void ToLower(Str& string)
	{
		std::transform(string.begin(), string.end(), string.begin(), static_cast<int(*)(int)>(std::tolower));
	}

	NewString(ToLower, ToLower)

	template<typename Str>
	void PadLeft(Str& str, const std::uint32_t width, const typename Str::value_type pad)
	{
		std::int64_t n = width - str.length();
		if (n <= 0) return;
		str.insert(str.begin(), n, pad);
	}

	NewString(PadLeft, PadLeft)

	template<typename Str>
	void PadRight(Str& str, const std::uint32_t width, const typename Str::value_type pad)
	{
		std::int64_t n = width - str.length();
		if (n <= 0) return;
		str.append(n, pad);
	}

	NewString(PadRight, PadRight)

	template<typename Str, typename...Args>
	void StringCombine(Str& str, Args&&... args)
	{
		(str.append(args), ...);
	}

	NewString(StringCombine, StringCombine)

	template<typename T, typename...Args>
	void FromStream(std::string& str, const T& stream, Args&&...fmt)
	{
		std::ostringstream buf{};
		(buf << ... << fmt) << stream;
		str.append(buf.str());
	}

	NewString(FromStream, FromStream)

	template<typename It, typename Str, typename Out>
	void Join(Out& str, It beg, It end, const Str& seq)
	{
		auto i = beg;
		if (end - beg == 0) return;
		for (; i < end - 1; ++i)
		{
			str.append(*i);
			str.append(seq);
		}
		str.append(*i);
	}

	NewString(Join, Join)

	template<typename Str, typename T>
	Str ToString(const T& t)
	{
		return __Detail::GetStrFunc<Str>()(__Detail::ToStringImpl(t));
	}

	namespace __Detail
	{
		template<typename StrVec, typename Arg>
		void ArgsToListImpl(StrVec& vec, const Arg& arg)
		{
			vec.push_back(ToString<typename StrVec::value_type>(arg));
		}

		template<typename StrVec, typename... Args>
		void ArgsToList(StrVec& out, Args&&...args)
		{
			(ArgsToListImpl(out, std::forward<Args>(args)), ...);
		}
	}

	template<typename Str>
	struct FormatTo
    {
        template<typename FmtStr, typename... Args>
        void operator()(Str& str, const FmtStr& fmtStr, Args&&...args)
        {
            std::vector<Str> argsStr{};
            const auto fmt = std::filesystem::path(fmtStr).u32string();
            __Detail::ArgsToList(argsStr, std::forward<Args>(args)...);
            const auto token = U"{}";
            auto start = 0;
            auto pos = fmt.find(token, start);
            uint64_t i = 0;
            while (pos != decltype(fmt)::npos)
            {
                str.append(ToString<Str>(fmt.substr(start, pos - start)));
                str.append(argsStr.at(i++));
                start = pos + 2;
                pos = fmt.find(token, start);
            }
            str.append(ToString<Str>(fmt.substr(start)));
        }
    };

    using FormatToStr = FormatTo<std::string>();
    using FormatToWstr = FormatTo<std::wstring>();
    using FormatToU16str = FormatTo<std::u16string>();
    using FormatToU32str = FormatTo<std::u32string>();

	NewStringImpl(FormatTo<std::string>{}, FormatStr, std::string)
	NewStringImpl(FormatTo<std::wstring>{}, FormatWstr, std::wstring)
	NewStringImpl(FormatTo<std::u16string>{}, FormatU16str, std::u16string)
	NewStringImpl(FormatTo<std::u32string>{}, FormatU32str, std::u32string)
}

#undef NewStringImpl
#undef NewString
