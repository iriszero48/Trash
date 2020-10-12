#include <cstdint>
#include <iostream>
#include <filesystem>
#include <string>
#include <charconv>
#include <unordered_map>
#include <execution>
#include <fstream>
#include <regex>
#include <deque>
#include <mutex>
#include <functional>
#include <algorithm>
#include <variant>

#include "Arguments.h"
#include "Convert.h"
#include "Macro.h"
#include "CSV.h"

ArgumentOption(MatchFlag, Line, Global)
ArgumentOption(SearchMode, Contains, Regex)
ArgumentOption(ExportFileType, Text, CSV)

static std::mutex LogMutex;
static std::mutex RecordMutex;
static bool Err = true;

template<typename Stream,  typename ...Args>
Stream& StreamCombine(Stream& stream, Args&&...args)
{
	std::lock_guard<std::mutex> lock(RecordMutex);
	(stream << ... << args);
	return stream;
}

template<typename ...Args>
std::ostringstream StringCombine(Args&&...args)
{
	std::ostringstream oss{};
	(oss << ... << args);
	return oss;
}

template<typename ...Args>
void SearchLog(Args&&...args)
{
	std::lock_guard<std::mutex> lock(LogMutex);
	(std::cout << ... << args) << '\n';
}

template<typename ...Args>
void SearchErrCombine(Args&&...args)
{
	if (Err)
	{
		std::lock_guard<std::mutex> lock(LogMutex);
		(std::cerr << ... << args) << '\n';
	}
}

#define SearchErr(...)\
	SearchErrCombine(__FILE__ ": " MacroLine ": " __FUNCTION__ ":\n", __VA_ARGS__)

struct SearchParams
{
	template<typename ...Ts> struct VisitFunc : Ts... { using Ts::operator()...; };
	template<typename ...Ts> VisitFunc(Ts...) -> VisitFunc<Ts...>;
	
	using ExportFileTextType = std::ofstream;
	using ExportFileCsvType = CSV::CsvFile;
	using ExportFileValueType = std::optional<std::variant<ExportFileTextType, ExportFileCsvType>>;
	SearchMode searchMode;
	MatchFlag matchFlag;
	bool ignoreCase;
	bool preLoad;
	int16_t border;
	ExportFileValueType& exportFile;
};

void SearchRecord(const std::string& file, const std::string& prefix, const std::string& keyword, const std::string& suffix, const SearchParams& searchParams)
{
#define __SearchRecord__Red_Text__(__text__) "\x1B[31m", __text__, "\033[0m"
	SearchLog(file, prefix, __SearchRecord__Red_Text__(keyword), suffix);
#define __SearchRecord__Stream_Log__(__stream__, __endl__) StreamCombine(__stream__, file, StringCombine(prefix, keyword, suffix).str(), __endl__)
	if (searchParams.exportFile.has_value())
	{
		std::visit(SearchParams::VisitFunc
		{
			[&](SearchParams::ExportFileTextType& arg) { __SearchRecord__Stream_Log__(arg, "\n"); },
			[&](SearchParams::ExportFileCsvType& arg) { __SearchRecord__Stream_Log__(arg, CSV::CsvFile::endl); },
		}, searchParams.exportFile.value());
	}
#undef __SearchRecord__Red_Text__
#undef __SearchRecord__Stream_Log__
}

void Search(const std::string& buf, const std::string& search, const SearchParams& searchParams, const std::string& prefix = "")
{
	try
	{
		if (searchParams.searchMode == SearchMode::Regex)
		{
			auto buf_ = buf;
			auto regexParams = std::regex::ECMAScript;
			if (searchParams.ignoreCase) regexParams |= std::regex::icase;
			const std::regex re(search, regexParams);
			for (std::regex_token_iterator<std::string::iterator> end, i(buf_.begin(), buf_.end(), re);
				i != end;
				*i++)
			{
				const auto pos = std::distance(buf_.begin(), i->first);
				SearchRecord(
					StringCombine(prefix, pos + 1, ": ").str(),
					StringCombine(
						pos > searchParams.border
							? buf_.substr(pos - searchParams.border, searchParams.border)
							: buf_.substr(0, pos)).str(),
					*i,
					StringCombine(buf_.substr(pos + i->length(), searchParams.border)).str(),
					searchParams);
			}
		}
		else
		{			
#define __Search__SearchMode_Contains__Loop__(__buf__, __search__, __str__)\
{\
	for (auto pos = (__buf__).find(__search__); pos != std::string::npos; pos = (__buf__).find((__search__), pos + 1))\
	{\
			SearchRecord(\
				StringCombine(prefix, pos + 1, ": ").str(),\
				StringCombine(\
					pos > searchParams.border\
						? (__str__).substr(pos - searchParams.border, searchParams.border)\
						: (__str__).substr(0, pos)).str(),\
				StringCombine((__str__).substr(pos, (__search__).length())).str(),\
				StringCombine((__str__).substr(pos + (__search__).length(), searchParams.border)).str(),\
				searchParams); \
	}\
}
			if (searchParams.ignoreCase)
			{
				auto toLower = [](auto& str) { std::transform(str.begin(), str.end(), str.begin(), static_cast<int(*)(int)>(std::tolower)); };
				auto buf_ = buf;
				auto search_ = search;
				toLower(buf_);
				toLower(search_);
				__Search__SearchMode_Contains__Loop__(buf_, search_, buf);
			}
			else
			{
				__Search__SearchMode_Contains__Loop__(buf, search, buf);
			}
		}
	}
	catch (const std::exception& e)
	{
		SearchErr(e.what());
	}
#undef __Search__SearchMode_Contains__Loop__
}

void SearchFile(
	const std::filesystem::path& path,
	const std::string& searchString,
	const SearchParams& searchParams)
{
	try
	{
		std::ifstream fs(path.string());
		const auto prefix = path.string() + ": ";
		if (searchParams.matchFlag == MatchFlag::Global)
		{
			std::ostringstream buf{};
			buf << fs.rdbuf();
			Search(buf.str(), searchString, searchParams, prefix);
		}
		else
		{

#define __SearchFile__MatchFlag_Line__Read_File_Line_By_Line_Loop__(__fs__, __loop__)\
{\
	auto i = 1;\
	for (std::string line; std::getline((__fs__), line); ++i)\
	{\
		__loop__;\
	}\
}

#define __SearchFile__MatchFlag_Line__Search_Caller__(__buf__, __index__)\
	Search(__buf__, searchString, searchParams, prefix + std::to_string(__index__) +": ")			
			if (searchParams.preLoad)
			{
				std::vector<std::tuple<int, std::string>> lines{};
				__SearchFile__MatchFlag_Line__Read_File_Line_By_Line_Loop__(fs, lines.emplace_back(i, line));
				std::for_each(std::execution::par_unseq, lines.begin(), lines.end(), [&](const auto& indexBuf)
				{
					__SearchFile__MatchFlag_Line__Search_Caller__(std::get<1>(indexBuf), std::get<0>(indexBuf));
				});
			}
			else
			{
				__SearchFile__MatchFlag_Line__Read_File_Line_By_Line_Loop__(fs, __SearchFile__MatchFlag_Line__Search_Caller__(line, i));
			}
		}
	}
	catch (const std::exception& e)
	{
		SearchErr("  ", e.what());
	}
#undef __SearchFile__MatchFlag_Line__Read_File_Line_By_Line_Loop__
#undef __SearchFile__MatchFlag_Line__Search_Caller__
}

void SearchDirectory(
	const std::filesystem::path& path,
	const std::string& searchString,
	const SearchParams& searchParams)
{
	std::list<std::filesystem::path> files{};
	std::error_code errorCode;
	const std::error_code nonErrorCode;
	std::deque<std::filesystem::path> queue{};
	const std::filesystem::directory_iterator end;
	queue.emplace_back(std::filesystem::path(path));
	while (!queue.empty())
	{
		try
		{
			for (std::filesystem::directory_iterator file(queue.front(), std::filesystem::directory_options::none, errorCode); file != end; ++file)
			{
				if (errorCode != nonErrorCode)
				{
					std::cerr << file->path().string() << " " << errorCode.message();
					errorCode.clear();
					continue;
				}
				if (file->is_symlink())
				{
					continue;
				}
				if (file->is_regular_file())
				{
					files.push_back(file->path());
				}
				else if (file->is_directory())
				{
					queue.emplace_back(file->path());
				}
			}
		}
		catch (const std::exception& e)
		{
			SearchErr("  ", queue.front().string(), ":\n    ", e.what());
		}
		queue.pop_front();
	}
	std::for_each(std::execution::par_unseq, files.begin(), files.end(), [&](const auto& file)
	{
		SearchFile(file, searchString, searchParams);
	});
}

int main(int argc, char* argv[])
{
	using Arguments::Argument;

#define InvalidArgument(v) Argument<>::ConstraintFuncMsg{ (v) + ": Invalid argument" }
#define InvalidArgumentFunc(func) [](const auto& v) { return (func) ? std::nullopt : InvalidArgument(v); }
#define NilConstraint [](auto) { return std::nullopt; }
	
	Arguments::Arguments args{};
	
	Argument searchStringArg(
		{},
		"search string",
		{},
		{ InvalidArgumentFunc(!v.empty()) });
	Argument searchPathArg(
		"-p",
		"directory path",
		{},
		{ InvalidArgumentFunc(std::filesystem::exists(v)) });
	Argument<SearchMode> modeArg(
		"-sm",
		"search mode " + SearchModeDesc(ToString(SearchMode::Contains)),
		{ SearchMode::Contains },
		{ NilConstraint },
		{ ToSearchMode });
	Argument<MatchFlag> flagArg(
		"-mf",
		"match flag " + MatchFlagDesc(ToString(MatchFlag::Line)),
		{ MatchFlag::Line },
		{ NilConstraint },
		{ ToMatchFlag });
	Argument<bool> ignoreCaseArg(
		"-ic",
		"ignore case [y|(n)]",
		{ false },
		{ NilConstraint },
		{ [](const auto& v) { return v == "y"; } });
	Argument<bool> preLoadArg(
		"-pl",
		"pre load [y|(n)]",
		{ false },
		{ NilConstraint },
		{ [](const auto& v) { return v == "y"; } });
	Argument<int16_t> borderArg(
		"-b",
		"border (50){0," MacroToString(INT16_MAX) "}",
		{ 50 },
		{ NilConstraint },
		{ [](const auto& v) { return Convert::ToInt(v); } });
	Argument<bool> errOutputArg(
		"-e",
		"err output [(y)|n]",
		{ true },
		{ NilConstraint },
		{ [](const auto& v) { return !(v == "n"); } });
	Argument exportPathArg(
		"-ep",
		"export path");
	Argument<ExportFileType> exportTypeArg(
		"-ef",
		"export type" + ExportFileTypeDesc(ToString(ExportFileType::CSV)),
		{ ExportFileType::CSV },
		{ NilConstraint },
		{ ToExportFileType });

	args.Add(searchStringArg);
	args.Add(searchPathArg);
	args.Add(modeArg);
	args.Add(flagArg);
	args.Add(ignoreCaseArg);
	args.Add(preLoadArg);
	args.Add(borderArg);
	args.Add(errOutputArg);
	args.Add(exportPathArg);
	args.Add(exportTypeArg);
	
//#define Debug
#ifndef Debug
	try
#endif
	{
#define Value(__arg__) args.Value<decltype(__arg__)::ValueType>(__arg__)
		args.Parse(argc, argv);
		Err = Value(errOutputArg);
		const auto path = Value(searchPathArg);
		SearchParams::ExportFileValueType exportFile = {};
		if (args.Get(exportPathArg).has_value())
		{
			const auto exportPath = Value(exportPathArg);
			switch (Value(exportTypeArg))
			{
			case ExportFileType::Text:
				exportFile = std::ofstream(exportPath);
				break;
			default:
				exportFile = CSV::CsvFile(exportPath);
				break;
			}
		}
		auto func = SearchDirectory;
		if (!std::filesystem::is_directory(path)) func = SearchFile;
		func(
			path,
			Value(searchStringArg),
			{
				Value(modeArg),
				Value(flagArg),
				Value(ignoreCaseArg),
				Value(preLoadArg),
				Value(borderArg),
				exportFile
			});
		
	}
#ifndef Debug
	catch (const std::exception& e)
	{
		SearchErr("  ", e.what());
	}
#endif
}
