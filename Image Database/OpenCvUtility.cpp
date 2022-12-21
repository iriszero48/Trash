#pragma once

#include "OpenCvUtility.h"

#include <fstream>

namespace OpenCvUtility
{
	std::optional<std::string> ReadToEnd(const std::filesystem::path& path)
	{
		std::string data;

		std::ifstream fs(path, std::ios::in | std::ios::binary);
		if (!fs) return std::nullopt;

		constexpr auto fsBufSize = 4096;
		const auto fsBuf = std::make_unique<char[]>(fsBufSize);
		fs.rdbuf()->pubsetbuf(fsBuf.get(), fsBufSize);

		while (!fs.eof())
		{
			constexpr auto bufSize = 4096;
			char buf[bufSize];
			fs.read(buf, bufSize);
			data.append(std::string_view(buf, fs.gcount()));
		}

		fs.close();

		return { data };
	}

	cv::Mat ReadImage(const std::filesystem::path& file)
	{
		const auto raw = *ReadToEnd(file);
		std::vector<char> data;
		std::copy_n(raw.begin(), raw.length(), std::back_inserter(data));
		return cv::imdecode(data, 1);
	}

	cv::Mat ReadImage(const char* data, const uint64 len)
	{
		std::vector<char> buf;
		std::copy_n(data, len, std::back_inserter(buf));
		return cv::imdecode(buf, 1);
	}

	cv::Mat ReadBlob(const cv::Mat& img)
	{
		return cv::dnn::blobFromImage(img, 1., cv::Size(224, 224), cv::Scalar(123.68, 116.779, 103.939), false);
	}

	cv::Mat ReadBlob(const char* data, const uint64 len)
	{
		return ReadBlob(ReadImage(data, len));
	}

	cv::Mat ReadBlob(const std::filesystem::path& file)
	{
		return ReadBlob(ReadImage(file));
	}
}
