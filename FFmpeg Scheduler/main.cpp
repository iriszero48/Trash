#include <cstdio>
#include <cstdlib>
#include <string>
#include <unordered_map>
#include <filesystem>
#include <regex>
#include <sstream>
#include <functional>
#include <numeric>

template<char SplitChar = ' '>
class StringForCombine : public std::string
{
public:
	StringForCombine(const char* str): std::string(str) { }
	StringForCombine(const std::string& str): std::string(str) { }

	friend std::ostream& operator<<(std::ostream& ss, const StringForCombine<SplitChar>& cs)
	{
		return ss << static_cast<std::string>(cs) << SplitChar;
	}
};

template<typename ...Args>
std::string Combine(Args&&... args)
{
	std::ostringstream ss;
	(ss << ... << args);
	return ss.str();
}

const StringForCombine HwCuvid = "-hwaccel cuvid";

const StringForCombine X264Cuvid = "-c:v h264_cuvid";
const StringForCombine X264Mmal = "-c:v h264_mmal";

const StringForCombine Input = R"(-i "$$$input$$$")";

const StringForCombine NvInput = Combine(HwCuvid, X264Cuvid, Input);

const StringForCombine copy = "-c copy";
const StringForCombine X264 = "-c:v libx264";
const StringForCombine X265 = "-c:v libx265";
const StringForCombine X264Nvenc = "-c:v h264_nvenc";
const StringForCombine X264Omx = "-c:v h264_omx";

const StringForCombine InputCopy = Combine(Input, copy);

const StringForCombine ScaleUp60Fps = R"(-filter:v "minterpolate='fps=60:mi_mode=mci:mc_mode=aobmc:me_mode=bidir:me=epzs:vsbmc=1:scd=fdiff'")";
const StringForCombine Size100X100 = "-s 100x100";

const StringForCombine PresetLossLessHp = "-preset losslesshp";
const StringForCombine PresetUltraFast = "-preset ultrafast";
const StringForCombine PresetSlower = "-preset slower";
const StringForCombine PresetVerySlow = "-preset veryslow";
const StringForCombine PresetPlacebo = "-preset placebo";

const StringForCombine Qp0 = "-qp 0";
const StringForCombine Crf14 = "-crf 14";
const StringForCombine Crf15 = "-crf 15";
const StringForCombine Crf17 = "-crf 17";

const StringForCombine Yuv420p10le = "-pix_fmt yuv420p10le";
const StringForCombine Yuv444p10le = "-pix_fmt yuv444p10le";

const StringForCombine ColorSpaceBt709 = "-colorspace 1";
const StringForCombine Bt709 = Combine(ColorSpaceBt709, "-color_primaries 1 -color_trc 1");
const StringForCombine Smpte170m = "-colorspace 6 -color_trc 6 -color_primaries 6";

const StringForCombine Anima60FpsAvcParams = R"(-x264-params "mbtree=1:aq-mode=3:psy-rd='0.6:0.15':aq-strength=0.8:rc-lookahead=180:qcomp=0.75:deblock='-1:-1':keyint=600:min-keyint=1:bframes=8:ref=13:me=tesa:no-fast-pskip=1")";
const StringForCombine Anima60FpsHevcParams = R"(-x265-params "deblock='-1:-1':ctu=32:qg-size=8:pbratio=1.2:cbqpoffs=-2:crqpoffs=-2:no-sao=1:me=3:subme=5:merange=38:b-intra=1:limit-tu=4:no-amp=1:ref=4:weightb=1:keyint=360:min-keyint=1:bframes=6:aq-mode=1:aq-strength=0.8:rd=5:psy-rd=2.0:psy-rdoq=1.0:rdoq-level=2:no-open-gop=1:rc-lookahead=180:scenecut=40:qcomp=0.65:no-strong-intra-smoothing=1:")";

const StringForCombine AvcLossLess = Combine(X264, PresetUltraFast, Qp0);
const StringForCombine AvcVisuallyLossLess = Combine(X264, Crf17);
const StringForCombine AvcVisuallyLossLessP10 = Combine(X264, Crf17, Yuv420p10le);
const StringForCombine NvencAvcLossLess = Combine(X264Nvenc, PresetLossLessHp, Qp0);
const StringForCombine AnimaAvcComp = Combine(X264, PresetVerySlow, Crf15, Yuv420p10le, Anima60FpsAvcParams);
const StringForCombine AnimaAvcCompYuv444 = Combine(X264, PresetVerySlow, Crf15, Yuv444p10le, Anima60FpsAvcParams);
const StringForCombine AnimaHevcComp = Combine(X265, PresetSlower, Crf14, Yuv420p10le, Anima60FpsHevcParams);

const StringForCombine Output = R"("$$$output$$$")";
const StringForCombine OutputJpg = R"("$$$output$$$.jpg")";
const StringForCombine OutputPng = R"("$$$output$$$.png")";
const StringForCombine OutputMp4 = R"("$$$output$$$.mp4")";


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

template<typename T = std::string>
class Argument
{
	using ConstraintFunc = std::function<bool(Argument<T>*, T)>;
	
	T value;
	std::string msg = "";
	ConstraintFunc constraint = [](auto x) { return true; };
	
public:
	const std::string name;
	const std::string desc;

	explicit Argument(const std::string& name, const std::string& desc = ""): name(name), desc(desc) { }

	explicit Argument(const std::string& name, ConstraintFunc constraint = [](auto x) { return true; }, const std::string& desc = ""): constraint(constraint), name(name), desc(desc) { }

	bool Set(const T& value)
	{
		return constraint(this, value);
	}

	[[nodiscard]] T Get()
	{
		return value;
	}
	
	[[nodiscard]] std::string GetMessage() const
	{
		return msg;
	}
};

class Arguments
{
	std::unordered_map<std::string, Argument<>> args;
public:
	template<typename ...T>
	void Add(T&&... arg)
	{
		Argument<> arg_(std::forward<T>(arg)...);
		args.emplace(arg_.name, arg_);
	}
	
	//void Add(const Argument<>& arg)
	//{
	//	args.insert(std::pair<std::string, Argument<>>(arg.name, arg));
	//}

	std::string GetDesc()
	{
		std::vector<std::string> desc{};
		std::transform(args.begin(), args.end(), desc, [](const auto& x)
		{
			std::ostringstream ss;
			ss << "    -" << x.first << " " << x.second.desc;
			return ss.str();
		});
		return std::accumulate(desc.begin(), desc.end(), "", [&](auto& a, auto& b) { a + " " + b; });
	}
	
	template<typename T>
	Argument<T> Get(const std::string& arg)
	{
		return args[args];
	}
	
	template<typename T>
	T operator[](const std::string& arg)
	{
		return args[arg].Get();
	}
};

int main(int argc, char* argv[])
{
	try
	{
		if (argc < 5)
		{
			fprintf(stderr, Combine(
				StringForCombine<'\n'>("%s [options]"),
				StringForCombine<'\n'>("    -i input directory"),
				StringForCombine<'\n'>("    -o output directory"),
				StringForCombine<'\n'>("    -t thread"),
				StringForCombine<'\n'>("    -c custom"),
				StringForCombine<'\n'>("    -m [(f)|d] file/directory"),
				StringForCombine<'\n'>("    -p preset")).c_str(), argv[0]);
			for (auto& preset : Preset) printf("        %s => %s\n", preset.first.c_str(), preset.second.c_str());
			exit(EXIT_FAILURE);
		}
		std::unordered_map<std::string, std::string> params
		{
			{"-i", ""},
			{"-o", ""},
			{"-p", ""},
			{"-t", ""},
			{"-c", ""},
			{"-m", "f"}
		};
		for (int i = 1; i < argc; i += 2) params[argv[i]] = argv[i + 1];

		const auto preset = params["-p"];
		const auto dir = std::filesystem::path(params["-i"]);
		const auto rawDir = dir / "raw";
		const auto doneDir = dir / "done";

		if (!exists(rawDir)) create_directory(rawDir);
		if (!exists(doneDir)) create_directory(doneDir);

		std::regex inputRe(R"(\${3}input\${3})");
		std::regex outputRe(R"(\${3}output\${3})");

		for (auto& file : std::filesystem::directory_iterator(
			     dir, std::filesystem::directory_options::skip_permission_denied))
		{
			if (!file.is_regular_file()) continue;
			auto currFile = dir / file.path().filename();
			auto cmd = "ffmpeg " + Preset[preset];
			cmd = std::regex_replace(cmd, inputRe, currFile.string());
			cmd = std::regex_replace(cmd, outputRe, (doneDir / file.path().filename()).string());
			printf("\n>>> %s\n\n", cmd.c_str());
			system(cmd.c_str());
			rename(currFile, rawDir / file.path().filename());
		}
	}
	catch (const std::exception& e)
	{
		fprintf(stderr, "Fatal Error: %s\n", e.what());
		exit(EXIT_FAILURE);
	}
}
