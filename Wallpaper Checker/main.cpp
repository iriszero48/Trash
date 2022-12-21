#include <filesystem>
#include <fstream>

#include <opencv2/opencv.hpp>

int main(int argc, char *argv[])
{
	const auto path = R"(C:\Users\iriszero\Documents\Share(CUnet)(auto_scale)(Level3)(tta)(x2.000000))";
	for (const auto &file : std::filesystem::recursive_directory_iterator(path))
	{
		if (!file.is_regular_file())
			continue;
		const auto size = file_size(file);
		std::vector<char> data;

		{
			const auto buf = std::make_unique<char[]>(size);
			std::ifstream fs(file, std::ios::in | std::ios::binary);
			constexpr auto fsBufSize = 4096;
			const auto fsBuf = std::make_unique<char[]>(fsBufSize);
			fs.rdbuf()->pubsetbuf(fsBuf.get(), fsBufSize);
			fs.read(buf.get(), size);
			std::copy_n(buf.get(), size, std::back_inserter(data));
		}

		const auto img = cv::imdecode(data, 1);
		const auto h = img.rows;
		const auto w = img.cols;
		std::cout << w << " x " << h << "\n";
		if (h >= w)
		{
			std::cout << file << " Kill.\n";
			std::filesystem::remove(file);
		}
		else
		{
			std::cout << file << (h > 1080 && w > 1920 ? " Well" : "") << " ok.\n";
		}
	}
}
