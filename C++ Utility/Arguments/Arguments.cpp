#include "Arguments.h"

#include <algorithm>
#include <queue>
#include <typeindex>

#define __Arguments_ToStringFunc__(x) #x
#define __Arguments_ToString__(x) __Arguments_ToStringFunc__(x)
#define __Arguments_Line__ __Arguments_ToString__(__LINE__)
#define __Arguments_ThrowEx__(...) throw std::runtime_error(__Arguments_Combine__( __FILE__ ": " __Arguments_Line__ ": ", __VA_ARGS__))

namespace ArgumentsParse
{
	std::string Arguments::GetDesc()
	{
		std::priority_queue<std::string, std::vector<std::string>, std::greater<>> item{};
		std::vector<std::string::size_type> lens{};
		std::transform(
			args.begin(),
			args.end(),
			std::back_inserter(lens),
			[&](const std::pair<std::string, IArgument*>& x) { item.push(x.first); return x.first.length(); });
		const auto maxLen = *std::max_element(lens.begin(), lens.end()) + 1;
		std::ostringstream ss;
		while (!item.empty())
		{
			const auto i = item.top();
			ss << __Arguments_Combine__(
				i, std::string(maxLen - i.length(), ' '),
				args.at(i)->GetDesc(), "\n");
			item.pop();
		}
		return ss.str();
	}
	std::string Arguments::GetValuesDesc(
		const std::unordered_map<std::type_index, std::function<std::string(std::any const&)>>& map)
	{
		std::priority_queue<std::string, std::vector<std::string>, std::greater<>> item{};
		std::vector<std::string::size_type> lens{};
		for (const auto& [k, _] : args)
		{
			if (args.at(k)->Get().type() != typeid(std::nullopt))
			{
				item.emplace(k);
				lens.push_back(k.length());
			}
		}
		const auto maxLen = *std::max_element(lens.begin(), lens.end());
		std::ostringstream ss;
		while (!item.empty())
		{
			const auto i = item.top();
			const auto vAny = args.at(i)->Get();
			ss << __Arguments_Combine__("Arguments [info]: ", i, std::string(maxLen - i.length(), ' '), ": ");
			std::string v;
			try
			{
				ss << map.at(vAny.type())(vAny);
			}
			catch (...)
			{
				ss << "unregistered type " << vAny.type().name();
			}
			ss << "\n";
			item.pop();
		}
		return ss.str();
	}

	void Arguments::Parse(const int argc, const char** argv)
	{
#define UnrecognizedOption(...) __Arguments_ThrowEx__("Unrecognized option: ", __VA_ARGS__)
#define MissingArgument(...) __Arguments_ThrowEx__("Missing argument for option: ", __VA_ARGS__)
		for (auto i = 1; i < argc; ++i)
		{
			auto pos = args.find(argv[i]);
			ArgLengthType def = 0;
			if (pos == args.end())
			{
				pos = args.find("");
				def = 1;
			}
			if (pos != args.end())
			{
				const auto len = pos->second->GetArgLength();
				if (len + i - def < argc)
				{
					auto setValue = [&]() -> IArgument::SetValueType {
						switch (len)
						{
						case 0:
							return { std::nullopt };
						case 1:
							return { argv[i + 1 - def] };
						default:
							std::vector<std::string_view> values{};
							for (auto j = 0; j < len; ++j)
							{
								values.emplace_back(argv[i + j + 1 - def]);
							}
							return { values };
						}
					}();
					pos->second->Set(setValue);
					i += len - def;
				}
				else
				{
					MissingArgument(argv[i]);
				}
			}
			else
			{
				UnrecognizedOption(argv[i]);
			}
		}
	}

	IArgument* Arguments::operator[](const std::string& arg)
	{
		return args.at(arg);
	}
}
