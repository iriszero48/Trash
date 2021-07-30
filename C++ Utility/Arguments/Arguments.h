#pragma once

#include <cstdint>
#include <string>
#include <any>
#include <functional>
#include <optional>
#include <sstream>
#include <typeindex>
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
	constexpr auto Version = "1.1.0";

	using ArgLengthType = std::uint8_t;

	template<ArgLengthType Len> struct SetValueTypeImp { using Type = std::vector<std::string_view>; };
	template<> struct SetValueTypeImp<1> { using Type = std::string_view; };
	template<> struct SetValueTypeImp<0> { using Type = std::nullopt_t; };

	class IArgument
	{
	public:
		using SetValueType = std::variant<SetValueTypeImp<0>::Type, SetValueTypeImp<1>::Type, SetValueTypeImp<2>::Type>;

		IArgument() = default;
		virtual ~IArgument() = default;
		IArgument(const IArgument& iArgument) = default;
		IArgument(IArgument&& iArgument) = default;
		IArgument& operator=(const IArgument& iArgument) = default;
		IArgument& operator=(IArgument&& iArgument) = default;

		virtual void Set(const SetValueType& value) = 0;
		virtual std::any Get() const = 0;
		virtual std::string GetName() const = 0;
		virtual std::string GetDesc() const = 0;
		virtual ArgLengthType GetArgLength() const = 0;
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
			ConvertFuncType convert = DefaultConvert) :
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

		[[nodiscard]] std::any Get() const override { return val; }

		[[nodiscard]] std::string GetName() const override { return name; }

		[[nodiscard]] std::string GetDesc() const override { return desc; }

		ArgLengthType GetArgLength() const override { return ArgLength; }

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
				__Arguments_ThrowEx__(args.at(arg.GetName())->GetName(), ": exists");
			}
			args[arg.GetName()] = &arg;
		}

		std::string GetDesc();

		template<typename T, typename F>
		std::pair<const std::type_index, std::function<std::string(std::any const&)>> GetValuesDescConverter(F const& f)
		{
			using OpT = std::optional<T>;
			return {
				std::type_index(typeid(OpT)),
				[g = f](std::any const& a)
				{
					return g(*std::any_cast<OpT const&>(a));
				}
			};
		}

		std::string GetValuesDesc(const std::unordered_map<std::type_index, std::function<std::string(std::any const&)>>& map);

		template <typename T, ArgLengthType Len>
		T Value(const Argument<T, Len>& arg) const
		{
			try
			{
				return Get<T, Len>(arg).value();
			}
			catch (const std::exception& e)
			{
				__Arguments_ThrowEx__(args.at(arg.GetName())->GetName(), ": ", e.what());
			}
		}

		template <typename T, ArgLengthType Len>
		std::optional<T> Get(const Argument<T, Len>& arg) const
		{
			return args.at(arg.GetName())->Get().type() == typeid(std::nullopt) ?
				std::nullopt :
				std::any_cast<std::optional<T>>(args.at(arg.GetName())->Get());
		}

		IArgument* operator[](const std::string& arg);

	private:
		std::unordered_map<std::string, IArgument*> args;
	};

#define ArgumentOptionHpp(option, ...)\
	enum class option { __VA_ARGS__ };\
	static auto __##option##_map__ = (([i = 0](std::string str) mutable\
	{\
		str.erase(std::remove(str.begin(), str.end(), ' '), str.end());\
		std::unordered_map<option, const std::string> res{};\
		std::string item;\
		std::stringstream stringStream(str);\
		while (std::getline(stringStream, item, ',')) res.emplace(static_cast<option>(i++), item);\
		return res;\
	})(#__VA_ARGS__));\
	std::string ToString(const option& in);\
	std::optional<option> To##option(const std::string& in);\
	std::string option##Desc(const std::string& defaultValue = "");

#define ArgumentOptionCpp(option)\
	std::string ToString(const option& in) { return __##option##_map__.at(in); }\
	std::optional<option> To##option(const std::string& in) { for (const auto& [k, v] : __##option##_map__) if (v == in) return k; return std::nullopt; }\
	std::string option##Desc(const std::string& defaultValue)\
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

#define ArgumentOption(option, ...)\
	ArgumentOptionHpp(option, __VA_ARGS__)\
	ArgumentOptionCpp(option)
}

#undef __Arguments_ToStringFunc__
#undef __Arguments_ToString__
#undef __Arguments_Line__
#undef __Arguments_ThrowEx__
