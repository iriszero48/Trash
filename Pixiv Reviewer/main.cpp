#include <filesystem>
#include <iostream>
#include <algorithm>
#include <execution>
#include <fstream>

#include <opencv2/opencv.hpp>

#include "Convert.h"
#include "Arguments.h"

struct Picture
{
    std::filesystem::path Path;
    std::uint64_t Pid = 0;

    explicit Picture(const std::filesystem::path& path): Path(path)
	{
        const auto str = path.filename().u8string();
        if (const auto start = str.find('('); start != decltype(str)::npos)
        {
	        if (const auto end = str.find(')'); end != decltype(str)::npos)
	        {
                Pid = *Convert::FromString<std::uint64_t>(str.substr(start + 1, end - start - 1));
	        }
        }
	}
};

int main(int argc, char* argv[])
{
    ArgumentsParse::Argument<std::filesystem::path> rootPathArg
    {
        "-r",
        "root path"
    };
    ArgumentsParse::Argument<std::filesystem::path> toMoveArg
    {
        "-d",
        "to move"
    };
    ArgumentsParse::Argument<std::uint64_t> startArg
    {
        "-s",
        "start pid",
        0,
        [](decltype(startArg)::ConvertFuncParamType value) -> decltype(startArg)::ConvertResult
        {
            return { *Convert::FromString<std::uint64_t>(std::string(value)), {} };
    	}
    };
    ArgumentsParse::Argument<bool, 0> onlyMoveArg
    {
        "--move",
        "only move",
        false,
        [](decltype(onlyMoveArg)::ConvertFuncParamType value) -> decltype(onlyMoveArg)::ConvertResult
        {
            return { true, {} };
    	}
    };

    ArgumentsParse::Arguments args;
    args.Add(rootPathArg);
    args.Add(toMoveArg);
    args.Add(startArg);
    args.Add(onlyMoveArg);
    //try
    //{
	    args.Parse(argc, argv);
    	
	    const auto rootPath = args.Value(rootPathArg);
	    const std::uint64_t startPid = args.Value(startArg);
	    const auto toMove = args.Value(toMoveArg);
	    const auto onlyMove = args.Value(onlyMoveArg);

		std::cout << args.GetValuesDesc({
			args.GetValuesDescConverter<std::filesystem::path>([](const auto& x) { return x.string(); }),
			args.GetValuesDescConverter<std::uint64_t>([](const auto& x) { return Convert::ToString(x).value(); }),
			args.GetValuesDescConverter<bool>([](const auto& x) { return x ? "true" : "false"; })
			});
    	
	    std::vector<Picture> pictures;
	
	    for (auto& file : std::filesystem::recursive_directory_iterator(rootPath))
	    {
		    Picture picture(file.path());
		    if (picture.Pid == 0) std::cerr << "error pid: " << picture.Path;
		    if (picture.Pid >= startPid)
		    {
			    pictures.push_back(picture);
		    }
	    }

	    std::sort(std::execution::par_unseq, pictures.begin(), pictures.end(), [](const Picture& p1, const Picture& p2) { return std::less()(p1.Pid, p2.Pid); });
	
	    for (const auto& [path, pid] : pictures)
	    {
		    if (onlyMove)
		    {
			    copy_file(path, toMove / path.filename());
			    continue;
		    }
    	
		    constexpr auto width = 960;
		    constexpr auto height = 1040 - 30;
		    const auto size = file_size(path);
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

		    const auto title = *Convert::ToString(pid);
    	
		    auto img = cv::imdecode(data, 1);
		    const auto fx = std::min(width * 1. / img.cols, height * 1. / img.rows);
		    cv::namedWindow(title);
		    cv::moveWindow(title, 0, 0);
		    cv::resize(img, img, cv::Size(), fx, fx, cv::InterpolationFlags::INTER_AREA);
		    cv::imshow(title, img);
		    cv::resizeWindow(title, width, height);
		    //std::cout << path << std::endl;
		    const auto key = cv::waitKey();
		    switch (key)
		    {
		    case 'q':
			    exit(EXIT_SUCCESS);
		    case ' ':
			    copy_file(path, toMove / path.filename());
		    default:
			    break;
		    }
		    cv::destroyWindow(title);
	    }
    //}
    //catch (const std::exception& ex)
    //{
	//	std::cerr << ex.what() << std::endl << args.GetDesc();
    //}
}
