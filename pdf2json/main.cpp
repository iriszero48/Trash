#include "Pdf2Json.h"
#include "Arguments.h"
#include "StdIO.h"
#include "Convert.h"

static_assert(ArgumentsParse::Version == std::string_view("1.1.0"));
static_assert(Console::Version == std::string_view("1.0.0"));
static_assert(MacroVersion == std::string_view("1.0.0"));
static_assert(Convert::Version == std::string_view("1.0.0"));

int main(int argc, char const *argv[])
{
#define ArgumentsFunc(arg) [&](decltype(arg)::ConvertFuncParamType value) -> decltype(arg)::ConvertResult
	
    ArgumentsParse::Argument<std::filesystem::path> inputArg
    {
        "-i",
        "input pdf file"
    };
    ArgumentsParse::Argument<std::filesystem::path> outputArg
    {
        "-o",
        "output dir"
    };
    ArgumentsParse::Argument<uint64_t> pageArg
    {
        "-p",
        "page(start with 0) [0," MacroToString(UINT64_MAX) "] ",
        ArgumentsFunc(pageArg) { return { *Convert::FromString<uint64_t>(value), {} }; }
    };

    ArgumentsParse::Arguments args;
    args.Add(inputArg);
    args.Add(outputArg);
    args.Add(pageArg);

    try
    {
        args.Parse(argc, argv);
        {
                Pdf2Json p2j(args.Value(inputArg), args.Value(pageArg));
                const auto res = p2j.SaveAsJson(args.Value(outputArg));
                Console::WriteLine(res.u8string());
        }
        {
            Pdf2Json p2j(args.Value(inputArg), args.Value(pageArg));
        p2j.SaveAsSvg(args.Value(outputArg));
        }
    }
    catch(const std::exception& e)
    {
        Console::Error::WriteLine(e.what());
        Console::Error::WriteLine(args.GetDesc());
    }
}
