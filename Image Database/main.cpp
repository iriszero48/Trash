#include <unordered_map>
#include <filesystem>
#include <utility>
#include <fstream>
#include <exception>
#include <optional>
#include <thread>
#include <string>
#include <coroutine>
#include <future>

#include <opencv2/opencv.hpp>
#include <zip.h>
#include <boost/context/continuation.hpp>

#include "Arguments.h"
#include "Convert.h"
#include "Cryptography.h"
#include "ImageDatabase.h"
#include "StdIO.h"
#include "Thread.h"
#include "Log.h"

ArgumentOption(Operator, build, query)

#define CatchEx

static Logger<std::wstring> Log;

int main(int argc, char* argv[])
{
#define ArgumentsFunc(arg) [&](decltype(arg)::ConvertFuncParamType value) -> decltype(arg)::ConvertResult
	
	ArgumentsParse::Argument<Operator> opArg
	{
		"",
		"operator " + OperatorDesc(),
		ArgumentsFunc(opArg) { return { *ToOperator(std::string(value)), {} }; }
	};
	ArgumentsParse::Argument<std::filesystem::path> dbArg
	{
		"-d",
		"database path"
	};
	ArgumentsParse::Argument<std::filesystem::path> pathArg
	{
		"-i",
		"input path"
	};
	ArgumentsParse::Argument<LogLevel> logLevelArg
	{
		"--loglevel",
		"log level " + LogLevelDesc(ToString(LogLevel::Info)),
		LogLevel::Info,
		ArgumentsFunc(logLevelArg) { return { *ToLogLevel(std::string(value)), {} };	}
	};
	ArgumentsParse::Argument<std::filesystem::path> logFileArg
	{
		"--logfile",
		"log level " + LogLevelDesc(ToString(LogLevel::Info)),
		""
	};
#undef ArgumentsFunc
	
	ArgumentsParse::Arguments args;
	args.Add(opArg);
	args.Add(dbArg);
	args.Add(pathArg);
	args.Add(logLevelArg);
	args.Add(logFileArg);

	std::thread logThread;
	
	cv::redirectError([](int status, const char* func_name, const char* err_msg,
		const char* file_name, int line, void*)
	{
		Log.Write<LogLevel::Error>(L"[OpenCV] " + std::filesystem::path(err_msg).wstring());
		return 0;
	});
	
#ifdef CatchEx
	try
#endif
	{
		args.Parse(argc, argv);

		logThread = std::thread([](const LogLevel& level, std::filesystem::path logFile)
		{
			Log.level = level;
			std::ofstream fs;
			if (!logFile.empty())
			{
				fs.open(logFile);
				const auto buf = std::make_unique<char[]>(4096);
				fs.rdbuf()->pubsetbuf(buf.get(), 4096);
				if (!fs)
				{
					Log.Write<LogLevel::Error>(L"log file: " + logFile.wstring() + L": open fail");
					logFile = "";
				}
			}

			while (true)
			{
				const auto [level, msg] = Log.Chan.Read();
				std::string out;
				String::StringCombine(out, "[", ToString(level), "] ");
				auto utf8 = false;
				try
				{
					String::StringCombine(out, std::filesystem::path(msg).string());
				}
				catch (...)
				{
					utf8 = true;
					String::StringCombine(out, std::filesystem::path(msg).u8string());
				}
				Console::WriteLine(out);
				if (!logFile.empty())
				{
					fs << (utf8 ? out : std::filesystem::path(out).u8string()) << std::endl;
					fs.flush();
				}

				if (level == LogLevel::None)
				{
					fs.close();
					break;
				}
			}
		}, args.Value(logLevelArg), args.Value(logFileArg));

		try
		{
			std::unordered_map<Operator, std::function<void()>>
			{
				{Operator::build, [&]()
				{
					const auto dbPath = args.Value(dbArg);
					const auto buildPath = args.Value(pathArg);
					ImageDatabase db(dbPath);

					ImageDatabase::Image img;
				
					boost::context::continuation source = boost::context::callcc(
						[&](boost::context::continuation&& sink)
						{
							for (const auto& file : std::filesystem::recursive_directory_iterator(buildPath))
							{
								if (file.is_regular_file())
								{
									const auto filePath = file.path();
									if (filePath.extension() == ".zip")
									{
										const auto zipFile = *OpenCvUtility::ReadToEnd(file.path());

										zip_error_t error;
										zip_source_t* src = zip_source_buffer_create(zipFile.data(), zipFile.length(), 0, &error);
										if (src == NULL && error.zip_err != ZIP_ER_OK)
										{
											Log.Write<LogLevel::Error>(std::wstring(L"load file: ") + file.path().wstring() + std::filesystem::path(error.str).wstring());
											continue;
										}

										zip_t* za = zip_open_from_source(src, ZIP_RDONLY, &error);
										if (za == NULL && error.zip_err != ZIP_ER_OK)
										{
											Log.Write<LogLevel::Error>(L"load file: " + file.path().wstring() + *Convert::ToWString(error.zip_err));
											continue;
										}

										const auto entries = zip_get_num_entries(za, 0);
										if (entries < 0)
										{
											Log.Write<LogLevel::Error>(L"load file: " + *Convert::ToWString(zip_get_error(za)->str));
											zip_close(za);
											continue;
										}

										for (int i = 0; i < entries; ++i)
										{
											struct zip_stat zs;
											if (zip_stat_index(za, i, 0, &zs) == 0)
											{
												if (const std::string_view filename(zs.name); filename[filename.length() - 1] != '/')
												{
													auto* const zf = zip_fopen_index(za, i, 0);
													if (zf == nullptr
														&& zip_get_error(za)->zip_err != ZIP_ER_OK)
													{
														Log.Write<LogLevel::Error>(L"load file: zip_fopen_index: fail");
														continue;
													}
													const auto buf = std::make_unique<char[]>(zs.size);
													if (zip_fread(zf, buf.get(), zs.size) < 0)
													{
														Log.Write<LogLevel::Error>(L"load file: zip_fread: fail");
														zip_fclose(zf);
														continue;
													}
													img = ImageDatabase::Image(file.path() / filename, std::string(buf.get(), zs.size));
													zip_fclose(zf);

													sink = sink.resume();
												}
											}
										}
										zip_close(za);
									}
									else if (filePath.extension() == ".gif")
									{
										cv::VideoCapture cap;
										std::string gifPath;
										bool copy = false;
										try
										{
											gifPath = file.path().string();
										}
										catch (...)
										{
											gifPath = String::StringCombineNew("tmp.", String::FromStreamNew(std::this_thread::get_id()), ".gif");
											try
											{
												copy_file(filePath, gifPath);
												copy = true;
											}
											catch (const std::exception& ex)
											{
												Log.Write<LogLevel::Error>(std::wstring(L"load file: ") + file.path().wstring() + L": copy_file fail: " + *Convert::ToWString(ex.what()));
												continue;
											}
										}
										if (!cap.open(gifPath))
										{
											Log.Write<LogLevel::Error>(std::wstring(L"load file: ") + file.path().wstring() + L": VideoCapture::Open fail");
											continue;
										}
										cv::Mat gif;
										for (uint64_t i = 0; cap.read(gif); ++i)
										{
											std::vector<uchar> rawData;
											imencode(".bmp", gif, rawData);
											std::string dataStr;
											std::copy_n(rawData.begin(), rawData.size(), std::back_inserter(dataStr));
											rawData.clear();
											rawData.shrink_to_fit();
											if (dataStr.empty())
											{
												Log.Write<LogLevel::Error>(std::wstring(L"load file: ") + file.path().wstring() + L": GIF: imencode fail");
												break;
											}
											const auto subPath = filePath / *Convert::ToString(i);
											Log.Write<LogLevel::Info>(L"load file: " + subPath.wstring());
											img = ImageDatabase::Image(subPath, dataStr);
											sink = sink.resume();
										}
										cap.release();
										if (copy) std::filesystem::remove(gifPath);
									}
									else
									{
										Log.Write<LogLevel::Info>(L"load file: " + file.path().wstring());
										img = ImageDatabase::Image(file.path());
										sink = sink.resume();
									}
								}
							}
							img = ImageDatabase::Image{};
							return std::move(sink);
						});

					for (uint64_t i = 0; !img.Path.empty(); source = source.resume(), ++i)
					{
						Log.Write<LogLevel::Info>(*Convert::ToWString(Convert::ToString(i)->c_str()) + L": compute md5: " + img.Path.wstring());
						img.ComputeMd5();
						Log.Write<LogLevel::Info>(L"compute vgg16: " + img.Path.wstring());
						try
						{
							img.ComputeVgg16();
						}
						catch (const cv::Exception& ex)
						{
							Log.Write<LogLevel::Error>(L"compute vgg16: " + img.Path.wstring() + L": " + *Convert::ToWString(ex.what()));
						}
						img.FreeMemory();
						db.Images.push_back(img);
					}

					Log.Write<LogLevel::Info>(L"save database: " + dbPath.wstring());
					db.Save(dbPath);
					Log.Write<LogLevel::Info>(L"database size: " + *Convert::ToWString(Convert::ToString(db.Images.size())->c_str()));
				}},
				{Operator::query, [&]()
				{
					const auto dbPath = args.Value(dbArg);
					const auto input = args.Value(pathArg);
					ImageDatabase db(dbPath);
					db.Load(dbPath);
					ImageDatabase::Image img(input);
					img.ComputeMd5();
					img.ComputeVgg16();

					std::sort(std::execution::par_unseq, db.Images.begin(), db.Images.end(), [&](const ImageDatabase::Image& a, const ImageDatabase::Image& b)
					{
						return std::greater()(a.Vgg16.dot(img.Vgg16), b.Vgg16.dot(img.Vgg16));
					});

					for (const auto& i : db.Images)
					{
						if (const auto v = i.Vgg16.dot(img.Vgg16); v >= 0.8f)
						{
							Log.Write<LogLevel::Log>(L"found " + std::filesystem::path(*Convert::ToString(v)).wstring() + L": " + i.Path.wstring());
						}
						else
						{
							break;
						}
					}
				}}
			}[args.Value(opArg)]();
		}
		catch (const std::exception& ex)
		{
			Log.Write<LogLevel::Error>(*Convert::ToWString(ex.what()));
		}

		Log.Write<LogLevel::None>(L"{ok}.");
		logThread.join();
	}
#ifdef CatchEx
	catch (const std::exception& e)
	{
		Console::Error::WriteLine(e.what());
		Console::Error::WriteLine(args.GetDesc());
	}
#endif
}
