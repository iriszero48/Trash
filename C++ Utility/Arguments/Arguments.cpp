#include "Arguments.h"

#define __Arguments_ToStringFunc__(x) #x
#define __Arguments_ToString__(x) __Arguments_ToStringFunc__(x)
#define __Arguments_Line__ __Arguments_ToString__(__LINE__)
#define __Arguments_ThrowEx__(...) throw std::runtime_error(__Arguments_Combine__( __FILE__ ": " __Arguments_Line__ ":\n", __VA_ARGS__))

namespace ArgumentsParse
{
	std::string Arguments::GetDesc()
	{
		std::ostringstream ss;
		for (auto& arg : args)
		{
			ss << __Arguments_Combine__(
				std::string(4, ' '),
				arg.first, " ",
				arg.second->GetDesc(), "\n");
		}
		return ss.str();
	}

	void Arguments::Parse(const int argc, const char** argv)
	{
		if (argc < 2)
		{
			__Arguments_ThrowEx__(argv[0], " [options]\n", GetDesc());
		}
		const auto defDefined = args.find("") != args.end();
#define UnrecognizedOption(...) __Arguments_ThrowEx__("Unrecognized option: ", __VA_ARGS__)
#define MissingArgument(...) __Arguments_ThrowEx__("Missing argument for option: ", __VA_ARGS__)
		for (auto i = 1; i < argc; ++i)
		{
			auto pos = args.find(argv[i]);
			if (pos != args.end())
			{
				const auto len = pos->second->GetArgLength();
				if (len + i < argc)
				{
					auto setValue = [&]() -> IArgument::SetValueType {
						switch (len)
						{
						case 0:
							return { std::nullopt };
						case 1:
							return { argv[i + 1] };
						default:
							std::vector<std::string_view> values{};
							for (auto j = 0; j < len; ++j)
							{
								values.emplace_back(argv[i + j + 1]);
							}
							return values;
						}
					}();
					pos->second->Set(setValue);
					i += len;
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
