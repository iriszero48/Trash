#include <filesystem>
#include <fstream>

#include <opencv2/opencv.hpp>

std::vector<std::filesystem::path> GetFiles(const std::filesystem::path& path)
{
	std::vector<std::filesystem::path> res;
	const auto it = std::filesystem::directory_iterator(path);
	std::copy(begin(it), end(it), std::back_inserter(res));
	return res;
}

int main(int argc, char* argv[])
{
	if (argc != 3)
	{
		std::cerr << argv[0] << " src dest" << std::endl;
		exit(EXIT_FAILURE);
	}
	const auto rootPath = argv[1];
	const auto copyTo = argv[2];
	for (const auto& file : std::filesystem::recursive_directory_iterator(rootPath))
	{
		if (!file.is_regular_file()) continue;
		const auto& path = file.path();
		constexpr auto width = 1920;
		constexpr auto height = 1040 - 30;
		const auto size = file_size(file);
		std::vector<char> data;

		{
			const auto buf = std::make_unique<char[]>(size);
			std::ifstream fs(path, std::ios::in | std::ios::binary);
			constexpr auto fsBufSize = 4096;
			const auto fsBuf = std::make_unique<char[]>(fsBufSize);
			fs.rdbuf()->pubsetbuf(fsBuf.get(), fsBufSize);
			fs.read(buf.get(), size);
			std::copy_n(buf.get(), size, std::back_inserter(data));
		}

		const auto title = file.path().u8string() + " " + std::to_string(GetFiles(file.path().parent_path()).size());
		auto img = cv::imdecode(data, 1);
		const auto fx = std::min(width * 1. / img.cols, height * 1. / img.rows);
		cv::namedWindow(title);
		cv::moveWindow(title, 0, 0);
		cv::resize(img, img, cv::Size(), fx, fx, fx > 1. ? cv::InterpolationFlags::INTER_LANCZOS4 : cv::InterpolationFlags::INTER_AREA);
		cv::imshow(title, img);
		cv::resizeWindow(title, img.cols, img.rows);
		switch (std::tolower(cv::waitKey()))
		{
		case 'q':
			exit(EXIT_SUCCESS);
		case ' ':
			copy_file(path, copyTo / path.filename());
		case 'd':
			std::filesystem::remove(path);
		default:
			break;
		}
		cv::destroyWindow(title);
	}
}
