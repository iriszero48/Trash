#pragma once

#define U8 (const char*)u8""
#define NormU8(u8) (const char8_t*)(u8)

namespace Text
{   
    enum class Language { English, Chinese };

    static auto GlobalLanguage = Language::Chinese;

#define MakeFunc(func) constexpr const char* func()
#define MakeText(lang, txt) if (lang == GlobalLanguage) return U8 txt

#define MakeEnText(txt) MakeText(Language::English, txt)
#define MakeCnText(txt) MakeText(Language::Chinese, txt); return "*undefined*"

    MakeFunc(InputPath)
    {
        MakeEnText("Input Path");
        MakeCnText("输入路径");
    }

    MakeFunc(OutputPath)
    {
        MakeEnText("Output Path");
        MakeCnText("输出路径");
    }

    MakeFunc(SelectSomething)
    {
        MakeEnText("Select...");
        MakeCnText("选择...");
    }

    MakeFunc(Export)
    {
        MakeEnText("Export");
        MakeCnText("导出");
    }

    MakeFunc(Start)
    {
        MakeEnText("Start");
        MakeCnText("开始");
    }

    MakeFunc(CurrentFile)
    {
        MakeEnText("Current File");
        MakeCnText("当前文件");
    }

    MakeFunc(Setting)
    {
        MakeEnText("Setting");
        MakeCnText("设置");
    }

    MakeFunc(BackgroundColor)
    {
        MakeEnText("Background Color");
        MakeCnText("背景颜色");
    }

    MakeFunc(RawImage)
    {
        MakeEnText("Raw Image");
        MakeCnText("原图");
    }

    MakeFunc(Preview)
    {
        MakeEnText("Preview");
        MakeCnText("预览");
    }

    MakeFunc(Finished)
    {
        MakeEnText("Finished");
        MakeCnText("已完成");
    }

    MakeFunc(File)
    {
        MakeEnText("File");
        MakeCnText("文件");
    }

    MakeFunc(Window)
    {
        MakeEnText("Window");
        MakeCnText("窗口");
    }

    MakeFunc(About)
    {
        MakeEnText("About");
        MakeCnText("关于");
    }

    MakeFunc(CubeFile)
    {
        MakeEnText("Cube File");
        MakeCnText("Cube文件");
    }

    MakeFunc(LinearDodgeColor)
    {
        MakeEnText("Linear Dodge(Color)");
        MakeCnText("线性添加(颜色)");
    }

    MakeFunc(Color)
    {
        MakeEnText("Color");
        MakeCnText("颜色");
    }

    MakeFunc(LinearDodgeImage)
    {
        MakeEnText("Linear Dodge(Image)");
        MakeCnText("线性添加(图片)");
    }

    MakeFunc(ImageFile)
    {
        MakeEnText("Image File");
        MakeCnText("图片文件");
    }

    MakeFunc(GenerateNormalTexture)
    {
        MakeEnText("Generate Normal Texture");
        MakeCnText("生成法线贴图");
    }

    MakeFunc(EditValue)
    {
        MakeEnText("EditValue");
        MakeCnText("编辑值");
    }

    MakeFunc(NormalMapFormatConvert)
    {
        MakeEnText("Normal Map Convert");
        MakeCnText("法线格式转换");
    }

    MakeFunc(InputFormat)
    {
        MakeEnText("Input Format");
        MakeCnText("输入格式");
    }

    MakeFunc(OutputFormat)
    {
        MakeEnText("Output Format");
        MakeCnText("输出格式");
    }

    MakeFunc(NowLoading)
    {
        MakeEnText("Now Loading...");
        MakeCnText("少女祈祷中...");
    }

    MakeFunc(Processor)
    {
        MakeEnText("Processor");
        MakeCnText("处理器");
    }

    MakeFunc(ProcessorPreview)
    {
        MakeEnText("Processor(Preview)");
        MakeCnText("处理器(预览)");
    }

    MakeFunc(ProcessorExport)
    {
        MakeEnText("Processor(Export)");
        MakeCnText("处理器(导出)");
    }

    MakeFunc(Error)
    {
        MakeEnText("Error");
        MakeCnText("错误");
    }

    MakeFunc(PathNotFound)
    {
        MakeEnText("Path Not Found");
        MakeCnText("路径不存在");
    }

    MakeFunc(Cancel)
    {
        MakeEnText("Cancel");
        MakeCnText("取消");
    }

    MakeFunc(Format)
    {
        MakeEnText("Format");
        MakeCnText("格式");
    }

#undef MakeFunc
#undef MakeText

#undef MakeEnText
#undef MakeCnText
}
