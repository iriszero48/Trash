#pragma once

#include <string>
#include <any>
#include <functional>
#include <optional>
#include <sstream>

namespace Arguments
{
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

	class IArgument
	{
	public:
		virtual ~IArgument() = default;
		virtual void Set(const std::string& value) = 0;
		virtual operator std::string() const = 0;
		virtual std::any Get() = 0;
		virtual std::string GetName() = 0;
		virtual std::string GetDesc() = 0;
	};

	template <typename T = std::string>
	class Argument : public IArgument
	{
	public:
		using ValueType = T;
		using ValueTypeOpt = std::optional<ValueType>;
		using ConstraintFuncMsg = std::optional<std::string>;
		using ConstraintFunc = std::function<ConstraintFuncMsg(const ValueType&)>;
		using ConvertFunc = std::function<ValueType(const std::string&)>;

		Argument(
			std::string name,
			std::string desc = "",
			ValueTypeOpt defaultValue = std::nullopt,
			ConstraintFunc constraint = [](const auto&) { return std::nullopt; },
			ConvertFunc convert = [](const auto& val) { return val; }) :
			val(std::move(defaultValue)),
			constraint(std::move(constraint)),
			convert(std::move(convert)),
			name(std::move(name)),
			desc(std::move(desc)) {}

		void Set(const std::string& value) override
		{
			auto conv = convert(value);
			auto msg = constraint(conv);
			if (!msg)
			{
				val = ValueTypeOpt(conv);
			}
			else
			{
				__Arguments_ThrowEx__(name, ": ", msg.value().c_str());
			}
		}

		[[nodiscard]] std::any Get() override { return val; }

		[[nodiscard]] std::string GetName() override { return name; }

		[[nodiscard]] std::string GetDesc() override { return desc; }

		[[nodiscard]] operator std::string() const override { return name; }

	private:
		std::any val;
		ConstraintFunc constraint;
		ConvertFunc convert;
		std::string name;
		std::string desc;
	};

	class Arguments
	{
	public:
		void Parse(int argc, char** argv);

		template <typename T>
		void Add(Argument<T>& arg)
		{
			if (args.find(arg.GetName()) != args.end())
			{
				__Arguments_ThrowEx__(args.at(arg)->GetName(), ": exists");
			}
			args[arg.GetName()] = &arg;
		}

		std::string GetDesc();

		template <typename T = std::string>
		typename Argument<T>::ValueType Value(const std::string& arg)
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
			return std::any_cast<typename Argument<T>::ValueTypeOpt>(args.at(arg)->Get());
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
