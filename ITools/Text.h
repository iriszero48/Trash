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
        MakeCnText("����·��");
    }

    MakeFunc(OutputPath)
    {
        MakeEnText("Output Path");
        MakeCnText("���·��");
    }

    MakeFunc(SelectSomething)
    {
        MakeEnText("Select...");
        MakeCnText("ѡ��...");
    }

    MakeFunc(Export)
    {
        MakeEnText("Export");
        MakeCnText("����");
    }

    MakeFunc(Start)
    {
        MakeEnText("Start");
        MakeCnText("��ʼ");
    }

    MakeFunc(CurrentFile)
    {
        MakeEnText("Current File");
        MakeCnText("��ǰ�ļ�");
    }

    MakeFunc(Setting)
    {
        MakeEnText("Setting");
        MakeCnText("����");
    }

    MakeFunc(BackgroundColor)
    {
        MakeEnText("Background Color");
        MakeCnText("������ɫ");
    }

    MakeFunc(RawImage)
    {
        MakeEnText("Raw Image");
        MakeCnText("ԭͼ");
    }

    MakeFunc(Preview)
    {
        MakeEnText("Preview");
        MakeCnText("Ԥ��");
    }

    MakeFunc(Finished)
    {
        MakeEnText("Finished");
        MakeCnText("�����");
    }

    MakeFunc(File)
    {
        MakeEnText("File");
        MakeCnText("�ļ�");
    }

    MakeFunc(Window)
    {
        MakeEnText("Window");
        MakeCnText("����");
    }

    MakeFunc(About)
    {
        MakeEnText("About");
        MakeCnText("����");
    }

    MakeFunc(CubeFile)
    {
        MakeEnText("Cube File");
        MakeCnText("Cube�ļ�");
    }

    MakeFunc(LinearDodgeColor)
    {
        MakeEnText("Linear Dodge(Color)");
        MakeCnText("�������(��ɫ)");
    }

    MakeFunc(Color)
    {
        MakeEnText("Color");
        MakeCnText("��ɫ");
    }

    MakeFunc(LinearDodgeImage)
    {
        MakeEnText("Linear Dodge(Image)");
        MakeCnText("�������(ͼƬ)");
    }

    MakeFunc(ImageFile)
    {
        MakeEnText("Image File");
        MakeCnText("ͼƬ�ļ�");
    }

    MakeFunc(GenerateNormalTexture)
    {
        MakeEnText("Generate Normal Texture");
        MakeCnText("���ɷ�����ͼ");
    }

    MakeFunc(EditValue)
    {
        MakeEnText("EditValue");
        MakeCnText("�༭ֵ");
    }

    MakeFunc(NormalMapFormatConvert)
    {
        MakeEnText("Normal Map Convert");
        MakeCnText("���߸�ʽת��");
    }

    MakeFunc(InputFormat)
    {
        MakeEnText("Input Format");
        MakeCnText("�����ʽ");
    }

    MakeFunc(OutputFormat)
    {
        MakeEnText("Output Format");
        MakeCnText("�����ʽ");
    }

    MakeFunc(NowLoading)
    {
        MakeEnText("Now Loading...");
        MakeCnText("��Ů����...");
    }

    MakeFunc(Processor)
    {
        MakeEnText("Processor");
        MakeCnText("������");
    }

    MakeFunc(ProcessorPreview)
    {
        MakeEnText("Processor(Preview)");
        MakeCnText("������(Ԥ��)");
    }

    MakeFunc(ProcessorExport)
    {
        MakeEnText("Processor(Export)");
        MakeCnText("������(����)");
    }

    MakeFunc(Error)
    {
        MakeEnText("Error");
        MakeCnText("����");
    }

    MakeFunc(PathNotFound)
    {
        MakeEnText("Path Not Found");
        MakeCnText("·��������");
    }

    MakeFunc(Cancel)
    {
        MakeEnText("Cancel");
        MakeCnText("ȡ��");
    }

    MakeFunc(Format)
    {
        MakeEnText("Format");
        MakeCnText("��ʽ");
    }

#undef MakeFunc
#undef MakeText

#undef MakeEnText
#undef MakeCnText
}
