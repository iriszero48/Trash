#include <filesystem>
#include <fstream>
#include <iostream>
#include <charconv>
#include <regex>
#include <array>
#include <optional>

#include <Arguments.hpp>
#include <String.hpp>

#define Ex(fmt, ...) std::runtime_error(String::Format("[{}:{}] [{}] {}", std::filesystem::path(__FILE__).filename(), __LINE__, __FUNCTION__, String::Format(fmt, __VA_ARGS__)))

std::string Hex(const uint8_t ch)
{
	std::string buf{'0', 0, 0};
	std::to_chars(buf.data() + 1, buf.data() + buf.length(), ch, 16);
	buf.erase(buf.begin() + (buf[2] == 0 ? 2 : 0));
	return buf;
}

std::string GetHead(const std::string &prefix, const uint64_t size, const std::filesystem::path &var)
{
	return String::Format("{} const unsigned char {}[{}]", prefix, var, size);
}

void WriteBody(const std::filesystem::path &output, const std::string head, const std::filesystem::path &input, const uint64_t size, const std::optional<std::string> include = {})
{
	std::ifstream fs(input, std::ios::in | std::ios::binary);
	if (!fs)
	{
		std::cerr << "fs.fail()" << std::endl;
	}

	std::ofstream out(output);
	std::array<char, 4096> buf;
	out.rdbuf()->pubsetbuf(buf.data(), buf.size());

	if (include.has_value())
	{
		out << include.value() << "\n";
	}

	out << head << " =\n{";

	for (std::uintmax_t i = 0; i < size; ++i)
	{
		char ch;
		fs.read(&ch, 1);

		out << (i % 16 == 0 ? "\n\t" : " ");

		out << "0x" << Hex(ch);
		if (i == size - 1)
		{
			out << "\n};";
			break;
		}
		else
		{
			out << ",";
		}
	}

	out.close();
	fs.close();
}

void Write(
	const std::filesystem::path &outDir,
	const std::filesystem::path &outFilename,
	const std::filesystem::path &input,
	const std::filesystem::path &var,
	const bool cpp)
{
	const auto hFilename = String::Format("{}.h", outFilename);
	const auto hPath = outDir / hFilename;
	const auto size = file_size(input);

	if (cpp)
	{
		const auto cppFilename = String::Format("{}.cpp", outFilename);
		const auto cppPath = outDir / cppFilename;
		const auto head = GetHead("extern", size, var);
		WriteBody(cppPath, head, input, size, String::Format(R"(#include "{}")", hFilename));

		std::ofstream fs(hPath);
		std::array<char, 4096> buf;
		fs.rdbuf()->pubsetbuf(buf.data(), buf.size());
		fs << head << ";";
		fs.close();

		return;
	}

	const auto head = GetHead("static", size, var);
	WriteBody(hPath, head, input, size);
}

int main(int argc, const char **argv)
{
	Args::Argument<std::filesystem::path, 1, true> inputPathArg("-i", "input path");
	Args::Argument<std::filesystem::path> outputPathArg("-o", "output dir path", ".");
	Args::Argument<std::filesystem::path> varNameArg("-v", "var name");
	Args::Argument<std::filesystem::path> filenameArg("-f", "file name without extension");
	Args::Argument<bool, 0> cppArg("-cpp", "use cpp file", false, [](auto)
								   { return true; });

	Args::Arguments args{};
	args.Add(inputPathArg, outputPathArg, varNameArg, filenameArg, cppArg);

	try
	{
		args.Parse(argc, argv);

		const auto input = args.Value(inputPathArg);
		if (!is_regular_file(input))
		{
			throw Ex("input is not a regular file");
		}

		const auto output = args.Value(outputPathArg);
		if (!is_directory(output))
		{
			throw Ex("output is not a directory");
		}

		const auto useCpp = args.Value(cppArg);
		std::regex re(R"(\W)");
		std::filesystem::path var = args
										.Get(varNameArg)
										.value_or(std::regex_replace((const char *)input.filename().u8string().c_str(), re, "_"));
		const auto filename = args.Get(filenameArg).value_or(var);

		std::filesystem::path outDir = output;
		std::filesystem::path outFilename = var;

		Write(output, filename, input, var, useCpp);
	}
	catch (const std::exception &ex)
	{
		std::cerr << ex.what() << std::endl
				  << args.GetDesc();
	}
}
