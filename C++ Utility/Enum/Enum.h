#pragma once

#include <stdexcept>
#include <string>
#include <unordered_map>

namespace Enum
{
    namespace __Detail
    {
        template <typename ...Args>
        std::string Combine(Args&&... args)
        {
            std::string buf{};
            (buf.append(args), ...);
            return buf;
        }

        template <typename T, size_t S>
		constexpr const char* GetFilename(const T (& str)[S], size_t i = S - 1)
		{
			for (; i > 0; --i) if (str[i] == '/' || str[i] == '\\') return &str[i + 1];
			return str;
		}
    }

    class Exception : std::runtime_error
    {
    public:
        template <typename ...Args>
        Exception(Args&&... args) : std::runtime_error(__Detail::Combine(std::forward<Args>(args)...)) {}
    };
}

#define __Enum_Str__(x) #x
#define __Enum_Ex__(func, ex, ...) ex("[", Enum::__Detail::GetFilename(__FILE__), ":", __Enum_Str__(__LINE__), "]", "[" func "]", "[" #ex "]", __VA_ARGS__)

#define Enum(name, ...)\
	enum class name { __VA_ARGS__ };\
	static auto __##name##_kv_map__ = (([i = 0](std::string str) mutable\
	{\
		str.erase(std::remove(str.begin(), str.end(), ' '), str.end());\
		std::unordered_map<name, const std::string> res{};\
		std::string item;\
		std::stringstream stringStream(str);\
		while (std::getline(stringStream, item, ',')) res.emplace(static_cast<name>(i++), item);\
		return res;\
	})(#__VA_ARGS__));\
	static auto __##name##_vk_map__ = []()\
	{\
		std::unordered_map<std::string, name> res{};\
		for (const auto& [k, v] : __##name##_kv_map__) res[v] = k;\
		return res;\
	}();\
	inline std::string EnumToString(const name& in)\
	{\
		try { return __##name##_kv_map__.at(in); }\
		catch (...) { std::throw_with_nested(\
            __Enum_Ex__("EnumToString", Enum::Exception, "convert to std::string fail")); } }\
	inline name EnumTo##name(const std::string& in)\
	{\
		try { return __##name##_vk_map__.at(in); }\
		catch (...) { std::throw_with_nested(\
			__Enum_Ex__("StringTo"#name, Enum::Exception, "convert to "#name" fail")); } }\
	static const std::vector<name>& Enum##name()\
	{\
		static const auto buf = []()\
		{\
			std::vector<name> buf;\
			for (const auto& k : __##name##_kv_map__) buf.push_back(k.first);\
			return buf;\
		}();\
		return buf;\
	}
