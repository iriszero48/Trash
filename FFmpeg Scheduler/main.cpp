#include <cstdio>
#include <cstdlib>
#include <string>
#include <unordered_map>
#include <filesystem>
#include <regex>
#include <sstream>
#include <functional>
#include <numeric>
#include <utility>
#include <optional>
#include <charconv>
#include <thread>
#include <chrono>
#include <exception>
#include <any>
#include <execution>

#define ToStringFunc(x) #x
#define ToString(x) ToStringFunc(x)

#define Line ToString(__LINE__)

#define ThrowEx(...) throw std::runtime_error(Combine( __FILE__ ": " Line ":\n", __VA_ARGS__))

#if (_WIN32 || _WIN64)

#define PathSeparator "\\"

#else

#define PathSeparator "/"

#endif

template <char Suffix = ' '>
class SuffixString : public std::string
{
public:
	template <typename ...T>
	SuffixString(T&&... arg): std::string(std::forward<T>(arg)...) { }

	friend std::ostream& operator<<(std::ostream& ss, const SuffixString<Suffix>& cs)
	{
		return ss << static_cast<std::string>(cs) << Suffix;
	}
};

template <char Prefix = ' '>
class PrefixString : public std::string
{
public:
	template <typename ...T>
	PrefixString(T&&... arg) : std::string(std::forward<T>(arg)...) { }

	friend std::ostream& operator<<(std::ostream& ss, const PrefixString<Prefix>& cs)
	{
		return ss << Prefix << static_cast<std::string>(cs);
	}
};

template <char Prefix = ' ', char Suffix = ' '>
class PrefixSuffixString : public std::string
{
public:
	template <typename ...T>
	PrefixSuffixString(T&&... arg) : std::string(std::forward<T>(arg)...) { }

	friend std::ostream& operator<<(std::ostream& ss, const PrefixSuffixString<Prefix, Suffix>& cs)
	{
		return ss << Prefix << static_cast<std::string>(cs) << Suffix;
	}
};

using DoubleQuotes = PrefixSuffixString<'\"', '\"'>;

template <typename ...Args>
std::string Combine(Args&&... args)
{
	std::ostringstream ss;
	(ss << ... << args);
	return ss.str();
}

#pragma region PresetElement

const SuffixString HwCuvid = "-hwaccel cuvid";

const SuffixString X264Cuvid = "-c:v h264_cuvid";
const SuffixString X264Mmal = "-c:v h264_mmal";

const SuffixString FrameRate29_97 = "-framerate 29.97";

const SuffixString Input = Combine("-i ", DoubleQuotes("$$$input$$$"));
const SuffixString InputPng_d = Combine("-i ", DoubleQuotes("$$$input$$$" PathSeparator "%d.png"));

const SuffixString NvInput = Combine(HwCuvid, X264Cuvid, Input);

const SuffixString copy = "-c copy";
const SuffixString X264 = "-c:v libx264";
const SuffixString X265 = "-c:v libx265";
const SuffixString X264Nvenc = "-c:v h264_nvenc";
const SuffixString X264Omx = "-c:v h264_omx";
const SuffixString Aac = "-c:a aac";

const SuffixString InputCopy = Combine(Input, copy);

const SuffixString LoudNorm = "-filter:a loudnorm";
const SuffixString ScaleUp60Fps = R"(-filter:v "minterpolate='fps=60:mi_mode=mci:mc_mode=aobmc:me_mode=bidir:me=epzs:vsbmc=1:scd=fdiff'")";
const SuffixString Image2 = "-f image2";

const SuffixString Size100X100 = "-s 100x100";
const SuffixString Size720p = "-s 1280x720";

const SuffixString PresetLossLessHp = "-preset losslesshp";
const SuffixString PresetUltraFast = "-preset ultrafast";
const SuffixString PresetSlower = "-preset slower";
const SuffixString PresetVerySlow = "-preset veryslow";
const SuffixString PresetPlacebo = "-preset placebo";

const SuffixString TuneFilm = "-tune film";

const SuffixString AudioBitrate128k = "-b:a 128k";

const SuffixString AudioS16 = "-sample_fmt s16";

const SuffixString Qp0 = "-qp 0";

const SuffixString Crf14 = "-crf 14";
const SuffixString Crf15 = "-crf 15";
const SuffixString Crf17 = "-crf 17";
const SuffixString Crf19 = "-crf 17";

const SuffixString Yuv420p = "-pix_fmt yuv420p";
const SuffixString Yuv420p10le = "-pix_fmt yuv420p10le";
const SuffixString Yuv444p10le = "-pix_fmt yuv444p10le";

const SuffixString ColorSpaceBt709 = "-colorspace 1";
const SuffixString Bt709 = Combine(ColorSpaceBt709, "-color_primaries 1 -color_trc 1");
const SuffixString Smpte170m = "-colorspace 6 -color_trc 6 -color_primaries 6";

const SuffixString Anima60FpsAvc720pParams = R"(-x264-params "deblock='0:0':keyint=600:min-keyint=1:ref=9:qcomp=0.7:rc-lookahead=180:aq-strength=0.9:merange=16:me=tesa:psy-rd='0:0.20':no-fast-pskip=1")";
const SuffixString Anima60FpsAvcParams = R"(-x264-params "mbtree=1:aq-mode=3:psy-rd='0.6:0.15':aq-strength=0.8:rc-lookahead=180:qcomp=0.75:deblock='-1:-1':keyint=600:min-keyint=1:bframes=8:ref=13:me=tesa:no-fast-pskip=1")";
const SuffixString Anima60FpsHevcParams = R"(-x265-params "deblock='-1:-1':ctu=32:qg-size=8:pbratio=1.2:cbqpoffs=-2:crqpoffs=-2:no-sao=1:me=3:subme=5:merange=38:b-intra=1:limit-tu=4:no-amp=1:ref=4:weightb=1:keyint=600:min-keyint=1:bframes=6:aq-mode=1:aq-strength=0.8:rd=5:psy-rd=2.0:psy-rdoq=1.0:rdoq-level=2:no-open-gop=1:rc-lookahead=180:scenecut=40:qcomp=0.65:no-strong-intra-smoothing=1:")";

const SuffixString AvcAudioComp = Combine(Aac, AudioBitrate128k/*, AudioS16*/);

const SuffixString AvcLossLess = Combine(X264, PresetUltraFast, Qp0);
const SuffixString AvcLossLessP10 = Combine(AvcLossLess, Yuv420p10le);
const SuffixString AvcVisuallyLossLess = Combine(X264, Crf17);
const SuffixString AvcVisuallyLossLessP10 = Combine(AvcVisuallyLossLess, Yuv420p10le);
const SuffixString NvencAvcLossLess = Combine(X264Nvenc, PresetLossLessHp, Qp0);
const SuffixString NvencAvc720pComp = Combine(X264Nvenc, PresetLossLessHp, Qp0);
const SuffixString AnimaAvc720pComp = Combine(X264, PresetVerySlow, Crf19, Yuv420p, TuneFilm, Anima60FpsAvc720pParams);
const SuffixString AnimaAvcComp = Combine(X264, PresetVerySlow, Crf15, Yuv420p10le, Anima60FpsAvcParams);
const SuffixString AnimaAvcCompYuv444 = Combine(X264, PresetVerySlow, Crf15, Yuv444p10le, Anima60FpsAvcParams);
const SuffixString AnimaHevcComp = Combine(X265, PresetSlower, Crf14, Yuv420p10le, Anima60FpsHevcParams);

const auto Output = DoubleQuotes("$$$output$$$");
const auto OutputJpg = DoubleQuotes("$$$output$$$.jpg");
const auto OutputPng = DoubleQuotes("$$$output$$$.png");
const auto OutputPng_d = DoubleQuotes("$$$input$$$" PathSeparator "%d.png");
const auto OutputMp4 = DoubleQuotes("$$$output$$$.mp4");

#pragma endregion PresetElement

std::unordered_map<std::string, std::string> Preset
{
	{"anima,avc,720p,comp,colorspace=bt709", Combine(InputCopy, Size720p, AvcAudioComp, AnimaAvc720pComp, ColorSpaceBt709, Output)},
	
	{"anima,avc,comp", Combine(InputCopy, AnimaAvcComp, Output)},
	{"anima,avc,comp,colorspace=bt709", Combine(InputCopy, AnimaAvcComp, ColorSpaceBt709, Output)},
	{"anima,avc,comp,bt709", Combine(InputCopy, AnimaAvcComp, Bt709, Output)},

	{"anima,hevc,comp", Combine(InputCopy, AnimaHevcComp, Output)},
	{"anima,hevc,comp,colorspace=bt709", Combine(InputCopy, AnimaHevcComp, ColorSpaceBt709, Output)},
	{"anima,hevc,comp,bt709", Combine(InputCopy, AnimaHevcComp, Bt709, Output)},

	{"anima,upto60fps,avc,ll", Combine(InputCopy, ScaleUp60Fps, AvcLossLessP10, Output)},
	{"anima,upto60fps,avc,ll,colorspace=bt709", Combine(InputCopy, ScaleUp60Fps, AvcLossLessP10, ColorSpaceBt709, Output)},
	{"anima,upto60fps,avc,ll,bt709", Combine(InputCopy, ScaleUp60Fps, AvcLossLessP10, Bt709, Output)},
	
	{"anima,upto60fps,avc,comp", Combine(InputCopy, ScaleUp60Fps, AnimaAvcComp, Output)},
	{"anima,upto60fps,avc,comp,colorspace=bt709", Combine(InputCopy, ScaleUp60Fps, AnimaAvcComp, ColorSpaceBt709, Output)},
	{"anima,upto60fps,avc,comp,bt709", Combine(InputCopy, ScaleUp60Fps, AnimaAvcComp, Bt709, Output)},

	{"anima,upto60fps,avc,comp,444p10,colorspace=bt709", Combine(InputCopy, ScaleUp60Fps, AnimaAvcCompYuv444, ColorSpaceBt709, Output)},

	{"avc,ll", Combine(InputCopy, AvcLossLess, Output)},
	{"avc,vll", Combine(InputCopy, AvcVisuallyLossLess, Output)},
	{"avc,vll,p10", Combine(InputCopy, AvcVisuallyLossLessP10, Output)},
	{"avc,vll,p10,bt709", Combine(InputCopy, AvcVisuallyLossLessP10, Bt709, Output)},
	{"avc,vll,p10,smpte170m", Combine(InputCopy, AvcVisuallyLossLessP10, Smpte170m, Output)},

	{"avc", Combine(Input, X264, Output)},
	{"avc,bt709", Combine(Input, X264, Bt709, Output)},
	{"avc,placebo", Combine(Input, X264, PresetPlacebo, Output)},

	{"avc,720p,nvenc,colorspace=bt709", Combine(InputCopy, Size720p, AvcAudioComp, X264Nvenc, Yuv420p, ColorSpaceBt709, Output)},
	
	{"nv,avc", Combine(NvInput, X264Nvenc, Output)},
	{"nv,avc,ll", Combine(NvInput, NvencAvcLossLess, Output)},

	{"rp,avc", Combine(X264Mmal, Input, X264Omx, Output)},

	{"pic,jpg", Combine(Input, OutputJpg)},
	{"pic,png", Combine(Input, OutputPng)},

	{"pic,png,resize100x100", Combine(Input, Size100X100, OutputPng)},

	{"vid,mp4", Combine(Input, OutputMp4)},

	{"vid2png%d", Combine(Input, Image2, OutputPng_d)},
	
	{"png%d2mp4,29.97fps,p10le", Combine(FrameRate29_97, InputPng_d, Yuv420p10le, Output)},

	{"loudnorm", Combine(InputCopy, LoudNorm, Output)},

	{"i", Combine(Input)}
};

[[nodiscard]] std::string PresetDesc()
{
	std::ostringstream ss;
	for (auto& preset : Preset)
	{
		ss << SuffixString<'\n'>(Combine(std::string(8, ' '), preset.first, " => ", preset.second));
	}
	return ss.str();
}

class Semaphore
{
public:
	Semaphore(const int count): count(count) {}

	void Release()
	{
		std::unique_lock<std::mutex> lock(mtx);
		count++;
		cv.notify_one();
	}

	void WaitOne()
	{
		std::unique_lock<std::mutex> lock(mtx);
		while (count == 0) cv.wait(lock);
		count--;
	}

private:
	std::mutex mtx;
	std::condition_variable cv;
	int count;
};

class IArgument
{
public:
	virtual ~IArgument() = default;
	virtual void Set(const std::string& value) = 0;
	virtual operator std::string() const = 0;
	virtual std::any Get() = 0;
	virtual std::string GetName() = 0;
	virtual std::string GetDesc() = 0;
};

template <typename T = std::string>
class Argument: public IArgument
{
public:
	using ValueType = T;
	using ValueTypeOpt = std::optional<ValueType>;
	using ConstraintFuncMsg = std::optional<std::string>;
	using ConstraintFunc = std::function<ConstraintFuncMsg(const ValueType&)>;
	using ConvertFunc = std::function<ValueType(const std::string&)>;

	Argument(
		std::string name,
		std::string desc = "",
		ValueTypeOpt defaultValue = std::nullopt,
		ConstraintFunc constraint = [](const auto&) { return std::nullopt; },
		ConvertFunc convert = [](const auto& val) { return val; }):
			val(std::move(defaultValue)),
			constraint(std::move(constraint)),
			convert(std::move(convert)),
			name(std::move(name)),
			desc(std::move(desc)) {}

	void Set(const std::string& value) override
	{
		auto conv = convert(value);
		auto msg = constraint(conv);
		if (!msg)
		{
			val = ValueTypeOpt(conv);
		}
		else
		{
			ThrowEx(name, ": ", msg.value().c_str());
		}
	}

	[[nodiscard]] std::any Get() override { return val; }

	[[nodiscard]] std::string GetName() override { return name; }

	[[nodiscard]] std::string GetDesc() override { return desc; }
	
	[[nodiscard]] operator std::string() const override { return name; }

private:
	std::any val;
	ConstraintFunc constraint;
	ConvertFunc convert;
	std::string name;
	std::string desc;
};

class Arguments
{
public:
	void Parse(const int argc, char** argv)
	{
		if (argc < 3 || (argc & 1) == 0)
		{
			ThrowEx(SuffixString(argv[0]), " [options]\n", GetDesc());
		}
		for (auto i = 1; i < argc; i += 2)
		{
			if (argv[i][0] != '-' || args.find(argv[i] + 1) == args.end())
			{
				ThrowEx(argv[i], ": Option not found");
			}
			args.at(argv[i] + 1)->Set(argv[i + 1]);
		}
	}

	template <typename T>
	void Add(Argument<T>& arg)
	{
		if (args.find(arg.GetName()) != args.end())
		{
			ThrowEx(PrefixString<'-'>(args.at(arg)->GetName()), ": exists");
		}
		args[arg.GetName()] = &arg;
	}

	std::string GetDesc()
	{
		std::ostringstream ss;
		for (auto& arg : args)
		{
			ss << SuffixString<'\n'>(Combine(SuffixString<'-'>(std::string(4, ' ')),
			                                 SuffixString(arg.first),
			                                 arg.second->GetDesc()));
		}
		return ss.str();
	}

	template <typename T = std::string>
	typename Argument<T>::ValueType Value(const std::string& arg)
	{
		try
		{
			return Get<T>(arg).value();
		}
		catch (const std::exception& e)
		{
			ThrowEx(PrefixString<'-'>(args.at(arg)->GetName()), ": ", e.what());
		}
	}

	template <typename T = std::string>
	std::optional<T> Get(const std::string& arg)
	{
		return std::any_cast<typename Argument<T>::ValueTypeOpt>(args.at(arg)->Get());
	}

	auto operator[](const std::string& arg)
	{
		return args.at(arg);
	}

private:
	std::unordered_map<std::string, IArgument*> args;
};

template<typename In = std::string, typename Out = int>
[[nodiscard]] Out Convert(const In& value)
{
	Out res;
	std::from_chars(value.data(), value.data() + value.size(), res);
	return res;
}

template<typename Element = std::filesystem::directory_entry, typename Container = std::vector<Element>>
[[nodiscard]] Container GetFiles(
	const std::filesystem::path& path,
	std::function<void(Container&, Element)> insertFunc = [](auto& files, const auto& file) { files.push_back(file); })
{
	Container files{};
	for (const auto& file :
		std::filesystem::directory_iterator(path, std::filesystem::directory_options::skip_permission_denied))
	{
		if (file.is_regular_file())
		{
			insertFunc(files, file);
		}
	}
	return files;
}

int main(int argc, char* argv[])
{
#define InvalidArgument(v) Argument<>::ConstraintFuncMsg{ Combine(v, ": Invalid argument") }
#define InvalidArgumentFunc(func) [](const auto& v) { return (func) ? std::nullopt : InvalidArgument(v); }

	try
	{
		Arguments args{};
		
		Argument input("i", "input");
		Argument output("o", "output");
		Argument log("l", "log path");
		Argument<int> thread(
			"t",
			"thread",
			1,
			{ InvalidArgumentFunc(v > 0) },
			Convert<std::string, int>);
		Argument custom("custom", "custom");
		Argument mode(
			"mode", 
			"[(f)|d] file/directory",
			{ "f" }, 
			{ InvalidArgumentFunc(v == "f" || v == "d") });
		Argument<bool> move(
			"move",
			"[(y)|n] move when done",
			{ true },
			{ InvalidArgumentFunc(true) },
			{ [](const auto& v) { return !(v == "n"); } });
		Argument call(
			"call",
			"(ffmpeg) call ffmpeg",
			{ "ffmpeg" });
		Argument inputExtension(
			"ie",
			"input extension",
			{ "" });
		Argument outputExtension(
			"oe",
			"output extension",
			{ "" });
		Argument preset(
			"p",
			Combine("preset\n", PresetDesc()),
			{},
			{ InvalidArgumentFunc(Preset.find(v) != Preset.end()) });
		
		args.Add(input);
		args.Add(output);
		args.Add(log);
		args.Add(thread);
		args.Add(custom);
		args.Add(mode);
		args.Add(move);
		args.Add(inputExtension);
		args.Add(outputExtension);
		args.Add(call);
		args.Add(preset);

		args.Parse(argc, argv);

		const std::regex inputRe(R"(\${3}input.?\${3})");
		const std::regex outputRe(R"(\${3}output\${3})");
		const std::regex inputExtensionRe(R"("\${3}output\${3}")");
		const std::regex outputExtensionRe(R"("\${3}output\${3}.*?")");

		const auto extendPresetCmd = std::regex_replace(
			std::regex_replace(
				Combine(SuffixString(args.Value(call)), args.Get(custom) ? args.Value(custom) : Preset[args.Value(preset)]),
				outputExtensionRe,
				Combine(SuffixString(args.Value(outputExtension)), "$&")),
			inputExtensionRe,
			Combine(SuffixString(args.Value(inputExtension)), "$&"));
		
		const auto inputPath = std::filesystem::path(args.Value(input));
		const auto outputPath = args.Get(output) ? std::filesystem::path(args.Value(output)) : inputPath / "done";
		
		create_directory(outputPath);
		
		if (args.Value(mode) == "f")
		{
			const auto moveWhenDone = args.Value<decltype(move)::ValueType>(move);
			const auto threadNum = args.Value<decltype(thread)::ValueType>(thread);
			
			const auto rawPath = inputPath / "raw";
			const auto logPath = args.Get(log) ? std::filesystem::path(args.Value(log)) : inputPath / "log";
			
			if (moveWhenDone) create_directory(rawPath);
			if (threadNum != 1) create_directory(logPath);

			auto files = GetFiles(inputPath);

			std::stable_sort(std::execution::par_unseq, files.begin(), files.end());
			
			std::mutex idMtx{};
			Semaphore cs(threadNum);

			auto ffmpeg = [&, count = 0](const auto& file) mutable
			{
				cs.WaitOne();
				idMtx.lock();
				auto id = count++;
				idMtx.unlock();

				const auto currFile = inputPath / file.path().filename();
				const auto cmd =
					Combine(
						std::regex_replace(
							std::regex_replace(
								extendPresetCmd,
								inputRe,
								currFile.string()),
							outputRe,
							(outputPath / file.path().filename()).string()),
						threadNum == 1 ? "" : Combine(" >", DoubleQuotes((logPath / Combine("log.", id)).string()), " 2>&1"));
				printf("\n>>> %s\n\n", cmd.c_str());
				system(cmd.c_str());
				
				if (moveWhenDone)
				{
					try
					{
						rename(currFile, rawPath / file.path().filename());
					}
					catch (...)
					{
						using namespace std::chrono_literals;
						std::this_thread::sleep_for(+1s);
						rename(currFile, rawPath / file.path().filename());
					}
				}
				cs.Release();
			};

			if (threadNum == 1)
			{
				if (args.Value<decltype(move)::ValueType>(move))
				{
					for (; !files.empty();
						files = GetFiles(inputPath),
						std::stable_sort(std::execution::par_unseq, files.begin(), files.end()))
					{
						ffmpeg(files[0]);
					}
				}
				else
				{
					std::for_each(files.begin(), files.end(), ffmpeg);
				}
			}
			else
			{
				std::for_each(std::execution::par, files.begin(), files.end(), ffmpeg);
			}
		}
		else
		{
			const auto cmd =
				std::regex_replace(
					std::regex_replace(
						extendPresetCmd,
						inputRe,
						inputPath.string()),
					outputRe,
					outputPath.string());
			printf("\n>>> %s\n\n", cmd.c_str());
			system(cmd.c_str());
		}
	}
	catch (const std::exception& e)
	{
		fputs(e.what(), stderr), exit(EXIT_FAILURE);
	}
}
// linux may need tbb
// g++ fs.cpp -o fs -std=c++17 -ltbb -O2
