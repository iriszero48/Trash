#pragma once

// #define UseOpenCV
#define UseStb

#include <climits>

#ifdef UseOpenCV
#include <opencv2/opencv.hpp>
#endif

extern "C"
{
#include <stb_image.h>
#include <stb_image_write.h>
}

namespace Image
{
#ifdef UseOpenCV
    namespace __Detail
    {
        template <typename T>
        constexpr auto ColorFloatTypeToCvType()
        {
            if constexpr (sizeof(T) * CHAR_BIT == 16) return CV_16FC4;
            else if constexpr (sizeof(T) * CHAR_BIT == 32) return CV_32FC4;
            else if constexpr (sizeof(T) * CHAR_BIT == 64) return CV_64FC4;
            else static_assert(false, "unknow float bit");
        }
    
        template <typename T> struct ColorTypeToCvType {};
        template <> struct ColorTypeToCvType<uint8_t> { constexpr auto operator()() { return CV_8UC4; } };
        template <> struct ColorTypeToCvType<int8_t> { constexpr auto operator()() { return CV_8SC4; } };
        template <> struct ColorTypeToCvType<uint16_t> { constexpr auto operator()() { return CV_16UC4; } };
        template <> struct ColorTypeToCvType<int16_t> { constexpr auto operator()() { return CV_16SC4; } };
        template <> struct ColorTypeToCvType<int32_t> { constexpr auto operator()() { return CV_32SC4; } };
        template <> struct ColorTypeToCvType<float> { constexpr auto operator()() { return ColorFloatTypeToCvType<float>(); } };
        template <> struct ColorTypeToCvType<double> { constexpr auto operator()() { return ColorFloatTypeToCvType<double>(); } };
        template <> struct ColorTypeToCvType<long double> { constexpr auto operator()() { return ColorFloatTypeToCvType<long double>(); } };
    }

    class ImageFile
    {
    private:
        cv::Mat data{};

    public:
        ImageFile() {}

        ImageFile(const std::filesystem::path& file)
        {
            LoadFile(file);
        }

        template <typename T>
        static ImageFile Create(const int64_t width, const int64_t height)
        {
            ImageFile img{};
            img.Data() = cv::Mat((int)height, (int)width, __Detail::ColorTypeToCvType<T>{}());
        
            return img;
        }

        void LoadFile(std::vector<char>& data)
        {
            data = cv::imdecode(data, cv::IMREAD_UNCHANGED);
        }

        void LoadFile(const std::filesystem::path& file)
        {
            try
            {
                data = cv::imread(file.string(), cv::IMREAD_UNCHANGED);
            }
            catch (...)
            {
                std::vector<char> fileData{};
            
                std::ifstream fs(file, std::ios::in | std::ios::binary);
                if (!fs) throw std::runtime_error("open image file failed");
            
                fs.read(fileData.data(), std::filesystem::file_size(file));
                fs.close();
            
               LoadFile(fileData);
            }
        }

        void Save(const std::filesystem::path& path)
        {
            try
            {
                data = cv::imwrite(path.string(), data);
            }
            catch (...)
            {
                std::vector<uint8_t> fileData{};
                cv::imencode(path.extension().string(), data, fileData);
            
                std::ofstream fs(path, std::ios::out | std::ios::binary);
                if (!fs) throw std::runtime_error("open image file failed");
            
                fs.write((const char*)fileData.data(), fileData.size());
                fs.close();
            }
        }

        cv::Mat& Data() { return data; }
        int Width() const { return data.cols; }
        int Height() const { return data.rows; }

        template <typename T>
        ColorRgba<T> At(const int64_t row, const int64_t col) const
        {
            const auto& bgra = data.at<cv::Vec4b>((int)row, (int)col);
            return ColorRgba<T>(bgra[RIdx], bgra[GIdx], bgra[BIdx], bgra[AIdx]);
        }

        template <typename T>
        void Set(const int64_t row, const int64_t col, const ColorRgba<T>& val)
        {
            //auto& bgra = data.at<cv::Vec4b>((int)row, (int)col);
            bgra[RIdx] = (uint8_t)val.R;
            bgra[GIdx] = (uint8_t)val.G;
            bgra[BIdx] = (uint8_t)val.B;
            bgra[AIdx] = (uint8_t)val.A;
        }

        struct MapParams
        {
            ColorRgba<uint8_t>& Color;
            int64_t row;
            int64_t col;
            const Image::ImageFile& img;
        };

    private:
        static const auto RIdx = 2;
        static const auto GIdx = 1;
        static const auto BIdx = 0;
        static const auto AIdx = 3;
    };
#endif

    template <typename T = uint8_t>
	struct ColorRgba
	{
		T R;
		T G;
		T B;
        T A;

		ColorRgba() : R(0), G(0), B(0), A(0) {}

		ColorRgba(const T v) : R(v), G(v), B(v), A(v) {}
        ColorRgba(const T v, const T a) : R(v), G(v), B(v), A(a) {}

		ColorRgba(const T r, const T g, const T b, const T a) : R(r), G(g), B(b), A(a) {}

		ColorRgba(const T (&arr)[4]) : R(arr[0]), G(arr[1]), B(arr[2]), A(arr[3]) {}
	};

#ifdef UseStb
    class ImageFile
    {
	    uint8_t* data = nullptr;
        int width = 0;
        int height = 0;
        bool autoFree = true;

    public:
        ImageFile() {}

        ImageFile(const std::filesystem::path& file)
        {
            LoadFile(file);
        }

        ImageFile(uint8_t* data, const int width, const int height, const bool autoFree = true) : data(data), width(width), height(height), autoFree(autoFree) {}

        ImageFile(const int width, const int height): width(width), height(height)
        {
            data = new uint8_t[width * height * 4];
        }

        ImageFile(const ImageFile& img)
        {
            width = img.width;
            height = img.height;
            autoFree = img.autoFree;
            data = new uint8_t[img.Size()];
            std::copy_n(img.data, img.Size(), data);
        }

        ImageFile(ImageFile&& img) noexcept
        {
            width = img.width;
            height = img.height;
            data = img.data;
            autoFree = img.autoFree;
            img.width = 0;
            img.height = 0;
            img.data = nullptr;
        }

        ImageFile& operator=(const ImageFile& img)
        {
            width = img.width;
            height = img.height;
            autoFree = img.autoFree;
            data = new uint8_t[img.Size()];
            std::copy_n(img.data, img.Size(), data);
            return *this;
        }

        ImageFile& operator=(ImageFile&& img)
        {
            width = img.width;
            height = img.height;
            data = img.data;
            autoFree = img.autoFree;
            img.width = 0;
            img.height = 0;
            img.data = nullptr;
            return *this;
        }

        ~ImageFile()
        {
            Clear();
        }

        ImageFile Copy() const
        {
            ImageFile buf(width, height);
            std::copy_n(data, Size(), buf.Data());
            return buf;
        }

        void Clear()
        {
            if (autoFree && data != nullptr) stbi_image_free(data);
            data = nullptr;
            width = 0;
            height = 0;
        }

        void LoadFile(const std::filesystem::path& file)
        {
            data = stbi_load(reinterpret_cast<const char*>(file.u8string().c_str()), &width, &height, nullptr, 4);
        }

        void Save(const std::filesystem::path& path) const
        {
            const auto ext = path.extension();
#define MakeIf(cond, func) if (cond) { func((const char*)path.u8string().c_str(), width, height, 4, data, width * 4); }
#define MakeData (const char*)path.u8string().c_str(), width, height, 4, data
            if (ext == ".png") stbi_write_png(MakeData, width * 4);
            else if (ext == ".jpg" || ext == ".jpeg" || ext == ".jpe") stbi_write_jpg(MakeData, 100);
            else if (ext == ".bmp") stbi_write_bmp(MakeData);
            else if (ext == ".tga") stbi_write_tga(MakeData);
            else throw std::runtime_error("unsupported format");
#undef MakeData
        }

        const uint8_t* Data() const { return data; }
    	uint8_t* Data() { return data; }
        int Width() const { return width; }
        int Height() const { return height; }
        std::size_t Size() const { return width * height * 4; }

        template <typename T>
        ColorRgba<T> At(const int64_t row, const int64_t col) const
        {
            const auto* p = data + (row * width + col) * 4;
            return ColorRgba<T>(p[0], p[1], p[2], p[3]);
        }

        template <typename T>
        void Set(const int64_t row, const int64_t col, const ColorRgba<T>& val)
        {
            auto* bgra = data + (row * width + col) * 4;
            bgra[RIdx] = static_cast<uint8_t>(val.R);
            bgra[GIdx] = static_cast<uint8_t>(val.G);
            bgra[BIdx] = static_cast<uint8_t>(val.B);
            bgra[AIdx] = static_cast<uint8_t>(val.A);
        }

        bool Empty() const
        {
            return data == nullptr || width == 0 || height == 0;
        }

    private:
        static constexpr auto RIdx = 0;
        static constexpr auto GIdx = 1;
        static constexpr auto BIdx = 2;
        static constexpr auto AIdx = 3;
    };
#endif
}