#include <filesystem>
#include <iostream>
#include <random>

#include "Arguments.h"

ArgumentOption(Filter, n, f, d)

template<bool Rec> struct Walker { using Type = std::filesystem::directory_iterator; };
template<> struct Walker<true> { using Type = std::filesystem::recursive_directory_iterator; };

template<Filter Dt, bool Rec>
std::vector<std::filesystem::path> __GetFiles_Impl_Impl__(const std::filesystem::path& path)
{
	std::vector<std::filesystem::path> files{};
	for (const auto& file : Walker<Rec>::Type(path))
	{
		if constexpr (Dt == Filter::f)
		{
			if (!file.is_regular_file()) continue;
		}
		else if constexpr (Dt == Filter::d)
		{
			if (!file.is_directory()) continue;
		}
		files.push_back(file.path());
	}
	return files;
}

template<Filter Dt>
std::vector<std::filesystem::path> __GetFiles_Impl__(const std::filesystem::path& path, bool rec = false)
{
	return
		rec ? __GetFiles_Impl_Impl__<Dt, true >(path):
			  __GetFiles_Impl_Impl__<Dt, false>(path);
}

std::vector<std::filesystem::path> GetFiles(const std::filesystem::path& path, const Filter filter = Filter::n, const bool rec = false)
{
	return
		filter == Filter::f ? __GetFiles_Impl__<Filter::f>(path, rec):
		filter == Filter::d ? __GetFiles_Impl__<Filter::d>(path, rec):
							  __GetFiles_Impl__<Filter::n>(path, rec);
}

int main(int argc, char* argv[])
{
#define ArgumentsFunc(arg) [&](decltype(arg)::ConvertFuncParamType value) -> decltype(arg)::ConvertResult
#define ArgumentsValue(arg) args.Value<decltype(arg)::ValueType>(arg)
	ArgumentsParse::Arguments args{};
	ArgumentsParse::Argument<std::filesystem::path> pathArg
	{
		{},
		"path"
	};
	ArgumentsParse::Argument<bool, 0> recArg
	{
		"-r",
		"rec walk",
		false,
		ArgumentsFunc(recArg)
		{
			return {true, {}};
		}
	};
	ArgumentsParse::Argument<bool, 0> execArg
	{
		"-e",
		"exec",
		false,
		ArgumentsFunc(execArg)
		{
			return {true, {}};
		}
	};
	ArgumentsParse::Argument<Filter> dataArg
	{
		"-f",
		"filter " + FilterDesc(ToString(Filter::n)),
		Filter::n,
		ArgumentsFunc(dataArg)
		{
			return {ToFilter(std::string(value)), {}};
		}
	};
	args.Add(pathArg);
	args.Add(recArg);
	args.Add(execArg);
	args.Add(dataArg);
	try
	{
		args.Parse(argc, argv);
		auto files = GetFiles(ArgumentsValue(pathArg), ArgumentsValue(dataArg), ArgumentsValue(recArg));
		std::random_device rd{};
		std::mt19937 mt(rd());
		std::shuffle(files.begin(), files.end(), mt);
		for (const auto& file : files)
		{
			try
			{
				const auto f = file.string();
				std::cout << f;
				if (ArgumentsValue(execArg))
				{
					std::cout << std::endl;
					system(("explorer /open,\"" + f + "\"").c_str());
				}
				std::string nil{};
				std::getline(std::cin, nil);
			}
			catch (const std::exception& ex)
			{
				std::cout << file.u8string() << " " << ex.what() << std::endl;
			}
		}
	}
	catch (const std::exception& ex)
	{
		std::cout << ex.what() << std::endl << args.GetDesc() << std::endl;
	}
}
