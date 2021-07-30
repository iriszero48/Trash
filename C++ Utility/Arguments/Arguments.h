#pragma once

#include <cstdint>
#include <string>
#include <any>
#include <functional>
#include <optional>
#include <sstream>
#include <variant>
#include <unordered_map>
#include <vector>

template <typename ...Args>
std::string __Arguments_Combine__(Args&&... args)
{
	std::ostringstream ss;
	(ss << ... << args);
	return ss.str();
}

#define __Arguments_ToStringFunc__(x) #x
#define __Arguments_ToString__(x) __Arguments_ToStringFunc__(x)

#define __Arguments_Line__ __Arguments_ToString__(__LINE__)

#define __Arguments_ThrowEx__(...) throw std::runtime_error(__Arguments_Combine__( __FILE__ ": " __Arguments_Line__ ":\n", __VA_ARGS__))

namespace ArgumentsParse
{
	constexpr auto Version = "1.0.1";

	using ArgLengthType = std::uint8_t;
	
	class IArgument
	{
	public:
		template<ArgLengthType Len> struct SetValueTypeImp { using Type = std::vector<std::string_view>; };
		template<> struct SetValueTypeImp<1> { using Type = std::string_view; };
		template<> struct SetValueTypeImp<0> { using Type = std::nullopt_t; };
		
		using SetValueType = std::variant<SetValueTypeImp<0>::Type, SetValueTypeImp<1>::Type, SetValueTypeImp<2>::Type>;

		IArgument() = default;
		virtual ~IArgument() = default;
		IArgument(const IArgument& iArgument) = default;
		IArgument(IArgument&& iArgument) = default;
		IArgument& operator=(const IArgument& iArgument) = default;
		IArgument& operator=(IArgument&& iArgument) = default;
		
		virtual void Set(const SetValueType& value) = 0;
		virtual operator std::string() const = 0;
		virtual std::any Get() = 0;
		virtual std::string GetName() = 0;
		virtual std::string GetDesc() = 0;
		virtual ArgLengthType GetArgLength() = 0;
	};
	
	template <typename T = std::string, ArgLengthType ArgLength = 1>
	class Argument final : public IArgument
	{
	public:
		using ValueType = T;
		using ValueTypeOpt = std::optional<ValueType>;

		struct ConvertResult
		{
			ValueTypeOpt Value;
			std::string Message;
		};

		using ConvertFuncParamTypeImp = typename SetValueTypeImp<ArgLength>::Type;
		using ConvertFuncParamType = const ConvertFuncParamTypeImp&;
		using ConvertFuncType = std::function<ConvertResult(ConvertFuncParamType)>;

		explicit Argument(
			std::string name,
			std::string desc,
			ValueTypeOpt defaultValue,
			ConvertFuncType convert = DefaultConvert):
			name(std::move(name)),
			desc(std::move(desc)),
			val(std::move(defaultValue)),
			convert(convert) {}

		explicit Argument(
			std::string name,
			std::string desc,
			ConvertFuncType convert = DefaultConvert) :
			name(std::move(name)),
			desc(std::move(desc)),
			convert(convert) {}

		void Set(const SetValueType& value) override
		{
			ConvertFuncParamTypeImp valueUnbox = std::get<decltype(valueUnbox)>(value);
			auto [v, e] = convert(valueUnbox);
			if (v.has_value())
			{
				val = v;
			}
			else
			{
				__Arguments_ThrowEx__(name, ": ", e.c_str());
			}
		}

		[[nodiscard]] std::any Get() override { return val; }

		[[nodiscard]] std::string GetName() override { return name; }

		[[nodiscard]] std::string GetDesc() override { return desc; }

		[[nodiscard]] operator std::string() const override { return name; }

		ArgLengthType GetArgLength() override { return ArgLength; }

		static ConvertResult DefaultConvert(ConvertFuncParamType value) { return { ValueTypeOpt(value), {} }; }

	private:
		std::string name;
		std::string desc;
		std::any val = std::nullopt;
		ConvertFuncType convert;
	};

	class Arguments
	{
	public:
		void Parse(int argc, const char** argv);

		template <typename T, ArgLengthType ArgLength>
		void Add(Argument<T, ArgLength>& arg)
		{
			if (args.find(arg.GetName()) != args.end())
			{
				__Arguments_ThrowEx__(args.at(arg)->GetName(), ": exists");
			}
			args[arg.GetName()] = &arg;
		}

		std::string GetDesc();

		template <typename T = std::string>
		T Value(const std::string& arg)
		{
			try
			{
				return Get<T>(arg).value();
			}
			catch (const std::exception& e)
			{
				__Arguments_ThrowEx__(args.at(arg)->GetName(), ": ", e.what());
			}
		}

		template <typename T = std::string>
		std::optional<T> Get(const std::string& arg)
		{
			return std::any_cast<std::optional<T>>(args.at(arg)->Get());
		}

		IArgument* operator[](const std::string& arg);

	private:
		std::unordered_map<std::string, IArgument*> args;
	};

#define ArgumentOption(option, ...)\
	enum class option { __VA_ARGS__ };\
	const auto __##option##_map__ = (([&, i = 0](std::string str) mutable\
	{\
		str.erase(std::remove(str.begin(), str.end(), ' '), str.end());\
		std::unordered_map<option, const std::string> res{};\
		std::string item;\
		std::stringstream stringStream(str);\
		while (std::getline(stringStream, item, ',')) res.emplace(static_cast<option>(i++), item);\
		return res;\
	})(#__VA_ARGS__));\
	std::string ToString(const option& in) { return __##option##_map__.at(in); }\
	option To##option(const std::string& in) { for (const auto& [k, v] : __##option##_map__) if (v == in) return k; }\
	std::string option##Desc(const std::string& defaultValue = "")\
	{\
		std::ostringstream oss{};\
		oss << "[";\
		for (const auto kv : __##option##_map__)\
		{\
			const auto sm = kv.second;\
			if (sm == defaultValue)\
			{\
				oss << "(" << sm << ")|";\
				continue;\
			}\
			oss << sm << "|";\
		}\
		auto res = oss.str();\
		res[res.length() - 1] = ']';\
		return res;\
	}
}

#undef __Arguments_ToStringFunc__
#undef __Arguments_ToString__
#undef __Arguments_Line__
#undef __Arguments_ThrowEx__
