#include "Arguments.h"

#define __Arguments_ToStringFunc__(x) #x
#define __Arguments_ToString__(x) __Arguments_ToStringFunc__(x)
#define __Arguments_Line__ __Arguments_ToString__(__LINE__)
#define __Arguments_ThrowEx__(...) throw std::runtime_error(__Arguments_Combine__( __FILE__ ": " __Arguments_Line__ ":\n", __VA_ARGS__))

namespace Arguments
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

	void Arguments::Parse(const int argc, char** argv)
	{
		if (argc < 2)
		{
			__Arguments_ThrowEx__(argv[0], " [options]\n", GetDesc());
		}
		const auto defDefined = args.find("") != args.end();
#define UnrecognizedOption(...) __Arguments_ThrowEx__("Unrecognized option: ", __VA_ARGS__)
#define MissingArgument(...) __Arguments_ThrowEx__("Missing argument for option: ", __VA_ARGS__)
		for (auto i = 1; i < argc; i += 2)
		{
			if (argv[i][0] == '-')
			{
				if(args.find(argv[i]) == args.end()) UnrecognizedOption(argv[i]);
				if (i + 1 >= argc) MissingArgument(argv[i]);
				args.at(argv[i])->Set(argv[i + 1]);
			}
			else if (defDefined)
			{
				args.at("")->Set(argv[i]);
				i--;
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
