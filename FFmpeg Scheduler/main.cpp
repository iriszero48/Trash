#include <cstdio>
#include <cstdlib>
#include <string>
#include <unordered_map>
#include <filesystem>
#include <regex>
#include <sstream>

class ArgString : public std::string
{
public:
	ArgString(const char* str):std::string(str){}
	ArgString(const std::string& str) :std::string(str) {}
	
	friend std::ostream& operator<< (std::ostream& ss, ArgString const& as)
	{
		return ss << as.c_str() << " ";
	}
};

template<typename ...Args>
std::string Combine(Args&&... args)
{
	std::ostringstream ss;
	(ss << ... << args) << "";
	return ss.str();
}

const ArgString Input = R"(-i "$$$input$$$")";

const ArgString HwCuvid = "-hwaccel cuvid";

const ArgString copy = "-c copy";

const ArgString CopyPrefixInput = Combine(Input, copy);

const ArgString X264Cuvid = "-c:v h264_cuvid";
const ArgString X264Mmal = "-c:v h264_mmal";

const ArgString NvInput = Combine(HwCuvid, X264Cuvid, Input);

const ArgString X264 = "-c:v libx264";
const ArgString X265 = "-c:v libx265";
const ArgString X264Nvenc = "-c:v h264_nvenc";
const ArgString X264Omx = "-c:v h264_omx";

const ArgString ScaleUp60Fps = R"(-filter:v "minterpolate='fps=60:mi_mode=mci:mc_mode=aobmc:me_mode=bidir:me=epzs:vsbmc=1:scd=fdiff'")";

const ArgString Size100X100 = "-s 100x100";

const ArgString PresetLossLessHp = "-preset losslesshp";

const ArgString PresetUltraFast = "-preset ultrafast";
const ArgString PresetSlower = "-preset slower";
const ArgString PresetVerySlow = "-preset veryslow";
const ArgString PresetPlacebo = "-preset placebo";

const ArgString Qp0 = "-qp 0";

const ArgString Crf14 = "-crf 14";
const ArgString Crf15 = "-crf 15";
const ArgString Crf17 = "-crf 17";

const ArgString Yuv420p10le = "-pix_fmt yuv420p10le";
const ArgString Yuv444p10le = "-pix_fmt yuv444p10le";

const ArgString ColorSpaceBt709 = "-colorspace 1";
const ArgString Bt709 = Combine(ColorSpaceBt709, "-color_primaries 1 -color_trc 1");
const ArgString Smpte170m = "-colorspace 6 -color_trc 6 -color_primaries 6";

const ArgString Anima60FpsAvcParams = R"(-x264-params "mbtree=1:aq-mode=3:psy-rd='0.6:0.15':aq-strength=0.8:rc-lookahead=180:qcomp=0.75:deblock='-1:-1':keyint=600:min-keyint=1:bframes=8:ref=13:me=tesa:no-fast-pskip=1")";
const ArgString Anima60FpsHevcParams = R"(-x265-params "deblock='-1:-1':ctu=32:qg-size=8:pbratio=1.2:cbqpoffs=-2:crqpoffs=-2:no-sao=1:me=3:subme=5:merange=38:b-intra=1:limit-tu=4:no-amp=1:ref=4:weightb=1:keyint=360:min-keyint=1:bframes=6:aq-mode=1:aq-strength=0.8:rd=5:psy-rd=2.0:psy-rdoq=1.0:rdoq-level=2:no-open-gop=1:rc-lookahead=180:scenecut=40:qcomp=0.65:no-strong-intra-smoothing=1:")";

const ArgString AvcLossLess = Combine(X264, PresetUltraFast, Qp0);
const ArgString AvcVisuallyLossLess = Combine(X264, Crf17);
const ArgString AvcVisuallyLossLessP10 = Combine(X264, Crf17, Yuv420p10le);
const ArgString NvencAvcLossLess = Combine(X264Nvenc, PresetLossLessHp, Qp0);
const ArgString AnimaAvcComp = Combine(X264, PresetVerySlow, Crf15, Yuv420p10le, Anima60FpsAvcParams);
const ArgString AnimaAvcCompYuv444 = Combine(X264, PresetVerySlow, Crf15, Yuv444p10le, Anima60FpsAvcParams);
const ArgString AnimaHevcComp = Combine(X265, PresetSlower, Crf14, Yuv420p10le, Anima60FpsHevcParams);

const ArgString Output = R"("$$$output$$$")";
const ArgString OutputJpg = R"("$$$output$$$.jpg")";
const ArgString OutputPng = R"("$$$output$$$.png")";
const ArgString OutputMp4 = R"("$$$output$$$.mp4")";


std::unordered_map<std::string, std::string> Preset
{
	{"anima,avc,comp", Combine(CopyPrefixInput, AnimaAvcComp, Output)},
	{"anima,avc,comp,colorspace=bt709", Combine(CopyPrefixInput, AnimaAvcComp, ColorSpaceBt709, Output)},
	{"anima,avc,comp,bt709", Combine(CopyPrefixInput, AnimaAvcComp, Bt709, Output)},

	{"anima,hevc,comp", Combine(CopyPrefixInput, AnimaHevcComp, Output)},
	{"anima,hevc,comp,colorspace=bt709", Combine(CopyPrefixInput, AnimaHevcComp, ColorSpaceBt709, Output)},
	{"anima,hevc,comp,bt709", Combine(CopyPrefixInput, AnimaHevcComp, Bt709, Output)},

	{"anima,upto60fps,avc,ll", Combine(CopyPrefixInput, ScaleUp60Fps, AvcLossLess, Output)},
	{"anima,upto60fps,avc,ll,colorspace=bt709", Combine(CopyPrefixInput, ScaleUp60Fps, AvcLossLess, ColorSpaceBt709, Output)},
	{"anima,upto60fps,avc,ll,bt709", Combine(CopyPrefixInput, ScaleUp60Fps, AvcLossLess, Bt709, Output)},
	
	{"anima,upto60fps,avc,comp", Combine(CopyPrefixInput, ScaleUp60Fps, AnimaAvcComp, Output)},
	{"anima,upto60fps,avc,comp,colorspace=bt709", Combine(CopyPrefixInput, ScaleUp60Fps, AnimaAvcComp, ColorSpaceBt709, Output)},
	{"anima,upto60fps,avc,comp,bt709", Combine(CopyPrefixInput, ScaleUp60Fps, AnimaAvcComp, Bt709, Output)},

	{"anima,upto60fps,avc,comp,444p10,colorspace=bt709", Combine(CopyPrefixInput, ScaleUp60Fps, AnimaAvcCompYuv444, ColorSpaceBt709, Output)},

	{"avc,ll", Combine(CopyPrefixInput, AvcLossLess, Output)},
	{"avc,vll", Combine(CopyPrefixInput, AvcVisuallyLossLess, Output)},
	{"avc,vll,p10", Combine(CopyPrefixInput, AvcVisuallyLossLessP10, Output)},
	{"avc,vll,p10,bt709", Combine(CopyPrefixInput, AvcVisuallyLossLessP10, Bt709, Output)},
	{"avc,vll,p10,smpte170m", Combine(CopyPrefixInput, AvcVisuallyLossLessP10, Smpte170m, Output)},
	
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

int main(int argc, char* argv[])
{
	if (argc < 3)
	{
		fprintf(stderr, "%s ...\n-d dir -p preset\n", argv[0]);
		for (auto& preset : Preset) printf("    %s => %s\n", preset.first.c_str(), preset.second.c_str());
		exit(EXIT_FAILURE);
	}
	std::unordered_map<std::string, std::string> params
	{
		{"-d", ""},
		{"-p", ""},
		{"-t", ""},
		{"-c", ""}
	};
	for (int i = 1; i < argc; i += 2) params[argv[i]] = argv[i + 1];

	const std::string preset = params["-p"];
	const auto dir = std::filesystem::path(params["-d"]);
	const auto rawDir = dir / "raw";
	const auto doneDir = dir / "done";
	
	if (!exists(rawDir)) create_directory(rawDir);
	if (!exists(doneDir)) create_directory(doneDir);

	std::regex inputRe(R"(\${3}input\${3})");
	std::regex outputRe(R"(\${3}output\${3})");

	for (auto& file : std::filesystem::directory_iterator(dir, std::filesystem::directory_options::skip_permission_denied))
	{
		if (!file.is_regular_file()) continue;
		auto currFile = dir / file.path().filename();
		auto cmd = "ffmpeg " + Preset[preset];
		cmd = std::regex_replace(cmd, inputRe, currFile.string());
		cmd = std::regex_replace(cmd, outputRe, (doneDir / file.path().filename()).string());
		printf("\n>>> %s\n", cmd.c_str());
		system(cmd.c_str());
		rename(currFile, rawDir / file.path().filename());
	}
}
