#include <filesystem>
#include <iostream>
#include <execution>

#include <opencv2/opencv.hpp>

int main(int argc, char* argv[])
{
	auto start = 3701;
	const auto inPath = R"(Z:\in)";
	const auto outPath = R"(Z:\out)";

	std::vector<std::tuple<int, std::string>> files{};
	for (auto& file : std::filesystem::directory_iterator(inPath))
	{
		files.emplace_back(start++, file.path().string());
	}

	std::for_each(std::execution::par_unseq, files.begin(), files.end(), [&](const auto& file)
	{
		const auto fi = std::get<0>(file);
		const auto& ff = std::get<1>(file);
		auto src = cv::imread(ff, cv::IMREAD_UNCHANGED);
		const int width = src.cols;
		const int height = src.rows;
		const auto hGridSize = height / 4;
		const auto wGridSize = width / 4;

		const std::filesystem::path dir = std::filesystem::path(outPath) / std::to_string(fi);
		create_directory(dir);
		auto i = 0;
		for (auto y = 0; y < height - hGridSize; y += hGridSize)
		{
			for (auto x = 0; x < width - wGridSize; x += wGridSize)
			{
				const auto filePath = dir / (std::to_string(i++) + ".jpg");
				cv::Rect gridRect(x, y, wGridSize, hGridSize);
				cv::imwrite(filePath.string(), src(gridRect));
			}
		}
		std::cout << dir << std::endl;
	});
}
