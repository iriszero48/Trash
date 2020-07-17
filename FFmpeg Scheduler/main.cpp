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

#define ToStringFunc(x) #x
#define ToString(x) ToStringFunc(x)

#define Line ToString(__LINE__)

#define ThrowEx(...) throw std::exception(Combine( __FILE__ ": " Line ":\n", __VA_ARGS__).c_str())

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

const SuffixString Input = R"(-i "$$$input$$$")";

const SuffixString NvInput = Combine(HwCuvid, X264Cuvid, Input);

const SuffixString copy = "-c copy";
const SuffixString X264 = "-c:v libx264";
const SuffixString X265 = "-c:v libx265";
const SuffixString X264Nvenc = "-c:v h264_nvenc";
const SuffixString X264Omx = "-c:v h264_omx";

const SuffixString InputCopy = Combine(Input, copy);

const SuffixString ScaleUp60Fps = R"(-filter:v "minterpolate='fps=60:mi_mode=mci:mc_mode=aobmc:me_mode=bidir:me=epzs:vsbmc=1:scd=fdiff'")";
const SuffixString Size100X100 = "-s 100x100";

const SuffixString PresetLossLessHp = "-preset losslesshp";
const SuffixString PresetUltraFast = "-preset ultrafast";
const SuffixString PresetSlower = "-preset slower";
const SuffixString PresetVerySlow = "-preset veryslow";
const SuffixString PresetPlacebo = "-preset placebo";

const SuffixString Qp0 = "-qp 0";
const SuffixString Crf14 = "-crf 14";
const SuffixString Crf15 = "-crf 15";
const SuffixString Crf17 = "-crf 17";

const SuffixString Yuv420p10le = "-pix_fmt yuv420p10le";
const SuffixString Yuv444p10le = "-pix_fmt yuv444p10le";

const SuffixString ColorSpaceBt709 = "-colorspace 1";
const SuffixString Bt709 = Combine(ColorSpaceBt709, "-color_primaries 1 -color_trc 1");
const SuffixString Smpte170m = "-colorspace 6 -color_trc 6 -color_primaries 6";

const SuffixString Anima60FpsAvcParams = R"(-x264-params "mbtree=1:aq-mode=3:psy-rd='0.6:0.15':aq-strength=0.8:rc-lookahead=180:qcomp=0.75:deblock='-1:-1':keyint=600:min-keyint=1:bframes=8:ref=13:me=tesa:no-fast-pskip=1")";
const SuffixString Anima60FpsHevcParams = R"(-x265-params "deblock='-1:-1':ctu=32:qg-size=8:pbratio=1.2:cbqpoffs=-2:crqpoffs=-2:no-sao=1:me=3:subme=5:merange=38:b-intra=1:limit-tu=4:no-amp=1:ref=4:weightb=1:keyint=360:min-keyint=1:bframes=6:aq-mode=1:aq-strength=0.8:rd=5:psy-rd=2.0:psy-rdoq=1.0:rdoq-level=2:no-open-gop=1:rc-lookahead=180:scenecut=40:qcomp=0.65:no-strong-intra-smoothing=1:")";

const SuffixString AvcLossLess = Combine(X264, PresetUltraFast, Qp0);
const SuffixString AvcVisuallyLossLess = Combine(X264, Crf17);
const SuffixString AvcVisuallyLossLessP10 = Combine(X264, Crf17, Yuv420p10le);
const SuffixString NvencAvcLossLess = Combine(X264Nvenc, PresetLossLessHp, Qp0);
const SuffixString AnimaAvcComp = Combine(X264, PresetVerySlow, Crf15, Yuv420p10le, Anima60FpsAvcParams);
const SuffixString AnimaAvcCompYuv444 = Combine(X264, PresetVerySlow, Crf15, Yuv444p10le, Anima60FpsAvcParams);
const SuffixString AnimaHevcComp = Combine(X265, PresetSlower, Crf14, Yuv420p10le, Anima60FpsHevcParams);

const SuffixString Output = R"("$$$output$$$")";
const SuffixString OutputJpg = R"("$$$output$$$.jpg")";
const SuffixString OutputPng = R"("$$$output$$$.png")";
const SuffixString OutputMp4 = R"("$$$output$$$.mp4")";

#pragma endregion PresetElement

std::unordered_map<std::string, std::string> Preset

{
	{"anima,avc,comp", Combine(InputCopy, AnimaAvcComp, Output)},
	{"anima,avc,comp,colorspace=bt709", Combine(InputCopy, AnimaAvcComp, ColorSpaceBt709, Output)},
	{"anima,avc,comp,bt709", Combine(InputCopy, AnimaAvcComp, Bt709, Output)},

	{"anima,hevc,comp", Combine(InputCopy, AnimaHevcComp, Output)},
	{"anima,hevc,comp,colorspace=bt709", Combine(InputCopy, AnimaHevcComp, ColorSpaceBt709, Output)},
	{"anima,hevc,comp,bt709", Combine(InputCopy, AnimaHevcComp, Bt709, Output)},

	{"anima,upto60fps,avc,ll", Combine(InputCopy, ScaleUp60Fps, AvcLossLess, Output)},
	{"anima,upto60fps,avc,ll,colorspace=bt709", Combine(InputCopy, ScaleUp60Fps, AvcLossLess, ColorSpaceBt709, Output)},
	{"anima,upto60fps,avc,ll,bt709", Combine(InputCopy, ScaleUp60Fps, AvcLossLess, Bt709, Output)},

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

	{"nv,avc", Combine(NvInput, X264Nvenc, Output)},
	{"nv,avc,ll", Combine(NvInput, NvencAvcLossLess, Output)},

	{"rp,avc", Combine(X264Mmal, Input, X264Omx, Output)},

	{"pic,jpg", Combine(Input, OutputJpg)},
	{"pic,png", Combine(Input, OutputPng)},

	{"pic,png,resize100x100", Combine(Input, Size100X100, OutputPng)},

	{"vid,mp4", Combine(Input, OutputMp4)}
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

template <typename T = std::string>
class Argument
{
public:
	using ConstraintFuncMsg = std::optional<std::string>;
	using ConstraintFunc = std::function<ConstraintFuncMsg(T)>;
	using ConvertFunc = std::function<T(std::string)>;

	std::string name;
	std::string desc;

	explicit Argument(
		std::string name,
		std::string desc = "",
		std::optional<T> defaultValue = {},
		ConstraintFunc constraint = [](auto) { return std::nullopt; },
		ConvertFunc convert = [](auto val) { return val; }):
			name(std::move(name)),
			desc(std::move(desc)),
			val(std::move(defaultValue)),
			constraint(std::move(constraint)),
			convert(std::move(convert)) { }

	void Set(const std::string& value)
	{
		auto conv = convert(value);
		auto msg = constraint(conv);
		if (!msg)
		{
			val = conv;
		}
		else
		{
			ThrowEx(name, ": ", msg.value().c_str());
		}
	}

	[[nodiscard]] std::optional<T> Get() { return val; }

	operator std::string() const { return name; }

private:
	std::optional<T> val;
	ConstraintFunc constraint;
	ConvertFunc convert;
};

class Arguments
{
public:
	void Parse(const int argc, char** argv)
	{
		if (argc < 3 || (argc & 1) == 0)
		{
			ThrowEx(SuffixString(argv[0]), " [options]\n", GetDesc(), PresetDesc());
		}
		for (auto i = 1; i < argc; i += 2)
		{
			if (argv[i][0] != '-' || args.find(argv[i] + 1) == args.end())
			{
				ThrowEx(argv[i], ": Option not found");
			}
			args.at(argv[i] + 1).Set(argv[i + 1]);
		}
	}

	template <typename ...T>
	void Add(T&&... args)
	{
		Argument<> arg(std::forward<T>(args)...);
		args.emplace(arg.name, arg);
	}

	template <typename T>
	void Add(const Argument<T> arg)
	{
		args.insert(std::pair<std::string, Argument<>>(arg.name, arg));
	}

	std::string GetDesc()
	{
		std::ostringstream ss;
		for (auto& arg : args)
		{
			ss << SuffixString<'\n'>(Combine(SuffixString<'-'>(std::string(4, ' ')),
			                                 SuffixString(arg.first),
			                                 arg.second.desc));
		}
		return ss.str();
	}

	template <typename T = std::string>
	T Value(const std::string& arg)
	{
		try
		{
			return args.at(arg).Get().value();
		}
		catch (const std::exception& e)
		{
			ThrowEx("-", args.at(arg).name, ": ", e.what());
		}
	}

	template <typename T = std::string>
	std::optional<T> Get(const std::string& arg)
	{
		return args.at(arg).Get();
	}

	Argument<> operator[](const std::string& arg)
	{
		return args.at(arg);
	}

private:
	std::unordered_map<std::string, Argument<>> args;
};

template<typename In = std::string, typename Out = int>
Out Convert(const In& value)
{
	Out res;
	std::from_chars(value.data(), value.data() + value.size(), res);
	return res;
}

int main(int argc, char* argv[])
{
#define UnrecognizedOption(v) Argument<>::ConstraintFuncMsg{ Combine(SuffixString("Unrecognized option:"), v) }
#define UnrecognizedOptionFunc(func) [](auto v) { return (func) ? std::nullopt : UnrecognizedOption(v); }
	
	try
	{
		Arguments args{};
		Argument input("i", "input");
		Argument output("o", "output");
		Argument<int> thread(
			"t",
			"thread",
			1,
			{ UnrecognizedOptionFunc(v > 0) },
			Convert<std::string, int>);
		Argument custom("c", "custom");
		Argument mode(
			"m", 
			"[(f)|d] file/directory",
			{ "f" }, 
			{ UnrecognizedOptionFunc(v == "f" || v == "d") });
		Argument preset(
			"p",
			"preset",
			{},
			{ UnrecognizedOptionFunc(Preset.find(v) == Preset.end()) });
		
		args.Add(input);
		args.Add(output);
		args.Add(thread);
		args.Add(custom);
		args.Add(mode);
		args.Add(preset);

		args.Parse(argc, argv);

		const auto presetCmd = args.Get(custom) ? args.Value(custom) : Combine("ffmpeg ", Preset[args.Value(preset)]);
		
		const auto inputPath = std::filesystem::path(args.Value(input));
		const auto outputPath = args.Get(output) ? args.Value(output) : inputPath / "done";

		std::regex inputRe(R"(\${3}input\${3})");
		std::regex outputRe(R"(\${3}output\${3})");

		if (!exists(outputPath)) create_directory(outputPath);
		
		if (args[mode].Get().value() == "f")
		{
			const auto rawPath = inputPath / "raw";
			if (!exists(rawPath)) create_directory(rawPath);
			
			for (auto& file : std::filesystem::directory_iterator(
				inputPath, std::filesystem::directory_options::skip_permission_denied))
			{
				if (!file.is_regular_file()) continue;
				auto currFile = inputPath / file.path().filename();
				auto cmd = Combine(SuffixString("ffmpeg"),
				                   std::regex_replace(
					                   std::regex_replace(presetCmd, inputRe,
					                                      currFile.string()), outputRe,
					                   (outputPath / file.path().filename()).string()));
				printf("\n>>> %s\n\n", cmd.c_str());
				system(cmd.c_str());
				try
				{
					rename(currFile, rawPath / file.path().filename());
				}
				catch (...)
				{
					using namespace std::chrono_literals;
					std::this_thread::sleep_for(1s);
					rename(currFile, rawPath / file.path().filename());
				}
			}
		}
		else
		{
			auto cmd = Combine(SuffixString("ffmpeg"),
				std::regex_replace(
					std::regex_replace(presetCmd, inputRe,
						inputPath.string()), outputRe,
					outputPath.string()));
			printf("\n>>> %s\n\n", cmd.c_str());
			system(cmd.c_str());
		}
	}
	catch (const std::exception& e)
	{
		fputs(e.what(), stderr), exit(EXIT_FAILURE);
	}
}
