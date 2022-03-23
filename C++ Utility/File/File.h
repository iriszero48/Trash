#pragma once

#include <string>
#include <filesystem>
#include <fstream>

namespace File
{
	constexpr int Version[]{ 1, 0, 0, 0 };

	inline void ReadAll(std::string& out, const std::filesystem::path& path)
	{
		std::ifstream fs(path, std::ios::in | std::ios::binary);
		if (!fs) throw std::runtime_error("[File::ReadToEnd] open file fail");

		constexpr auto fsBufSize = 4096;
		char fsBuf[4096]{ 0 };
		fs.rdbuf()->pubsetbuf(fsBuf, fsBufSize);

		while (!fs.eof())
		{
			constexpr auto bufSize = 4096;
			char buf[bufSize];
			fs.read(buf, bufSize);
			out.append(std::string(buf, fs.gcount()));
		}

		fs.close();
	}

    inline std::string ReadAll(const std::filesystem::path& path)
	{
		std::string out{};
		ReadAll(out, path);
		return out;
	}

    inline void ReadLines(std::vector<std::string> lines, const std::filesystem::path& path)
    {
        std::ifstream fs(path, std::ios::in);
        if (!fs) throw std::runtime_error("[File::ReadLines] open file fail");

        std::string line;
        while (std::getline(fs, line)) lines.push_back(line);

        fs.close();
    }

    inline std::vector<std::string> ReadLines(const std::filesystem::path& path)
    {
        std::vector<std::string> lines;
        ReadLines(lines, path);
        return lines;
    }

    inline void WriteAll(const std::filesystem::path& path, const std::string_view& data)
	{
		std::ofstream fs(path, std::ios::out | std::ios::binary);
		if (!fs) throw std::runtime_error("[File::WriteAll] open file failed");

		constexpr auto fsBufSize = 4096;
		char fsBuf[4096] { 0 };
		fs.rdbuf()->pubsetbuf(fsBuf, fsBufSize);

		fs.write(data.data(), data.length());
		fs.close();
	}
}