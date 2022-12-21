#pragma once

#include <filesystem>
#include <optional>
#include <string>

#include <opencv2/opencv.hpp>

namespace OpenCvUtility
{
	std::optional<std::string> ReadToEnd(const std::filesystem::path& path);

	cv::Mat ReadImage(const std::filesystem::path& file);

	cv::Mat ReadImage(const char* data, const uint64 len);

	cv::Mat ReadBlob(const cv::Mat& img);

	cv::Mat ReadBlob(const char* data, const uint64 len);

	cv::Mat ReadBlob(const std::filesystem::path& file);
}
