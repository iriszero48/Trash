#pragma once

#include "Image.h"

#undef max
#undef min

namespace ImageTools
{
	namespace __Detail
	{
        template<class... Ts> struct Visitor : Ts... { using Ts::operator()...; };

        template <typename T>
        T blerp(T c00, T c10, T c01, T c11, T tx, T ty)
        {
            return std::lerp(std::lerp(c00, c10, tx), std::lerp(c01, c11, tx), ty);
        }

        template <typename T>
        T clerp(T c000, T c010, T c100, T c110, T c001, T c011, T c101, T c111, T tx, T ty, T tz)
        {
            return std::lerp(blerp(c000, c100, c010, c110, tx, ty), blerp(c001, c101, c011, c111, tx, ty), tz);
        }
	}

    struct ImageSize
    {
        int Width;
        int Height;
    };

    template <typename Impl>
    class ITool
    {
	public:

        const Image::ImageFile* _ImgRef = nullptr;

        void ImgRef(const Image::ImageFile& img)
        {
        	static_cast<Impl*>(this)->ImgRef(img);
        }

        ImageSize GetOutputSize() const
        {
            return static_cast<Impl*>(this)->Size();
        }

        Image::ColorRgba<uint8_t> operator()(int64_t row, int64_t col)
        {
            return static_cast<Impl*>(this)->At(row, col);
        }
    };

    class LUT: ITool<LUT>
    {
	    Lut::CubeLut lutData;
        
        Lut::CubeLut::Row dMax;
        Lut::CubeLut::Row dMin;
        float size;

        template <typename T>
        Lut::CubeLut::Row SafeAt(const Lut::CubeLut& tab, const T b, const T g, const T r, const T i = 0)
        {
            return std::visit(__Detail::Visitor
                {
                    [&](const Lut::Table1D& t1)
                    {
                        if (i >= static_cast<T>(t1.Length()) || i < static_cast<T>(0)) return tab.DomainMin;
                        return t1.At(i);
                    },
                    [&](const Lut::Table3D& t3)
                    {
                        if (std::max(b, std::max(g, r)) >= static_cast<T>(t3.Length()) ||
                            std::min(b, std::min(g, r)) < static_cast<T>(0))
                            return tab.DomainMin;
                        return t3.At(r, g, b);
                    }
                }, tab.GetTable());
        }

        template <typename T>
        Lut::ColorRgb<T> LookUp(const Lut::CubeLut& lut, const T b, const T g, const T r)
        {
            return std::visit(__Detail::Visitor
                {
                    [&](const Lut::Table1D& tab)
                    {
                        int64_t bi = static_cast<int64_t>(b), gi = static_cast<int64_t>(g), ri = static_cast<int64_t>(r);
                        throw std::runtime_error("not impl");
                        return Lut::ColorRgb<T>(0, 0, 0);
                    },
                    [&](const Lut::Table3D& tab)
                    {
                        int64_t bi = static_cast<int64_t>(b), gi = static_cast<int64_t>(g), ri = static_cast<int64_t>(r);
                        const Lut::CubeLut::Row&
                            c000 = SafeAt(lut, bi,     gi,     ri),
                            c010 = SafeAt(lut, bi,     gi + 1, ri),
                            c100 = SafeAt(lut, bi + 1, gi,     ri),
                            c110 = SafeAt(lut, bi + 1, gi + 1, ri),
                            c001 = SafeAt(lut, bi,     gi,     ri + 1),
                            c011 = SafeAt(lut, bi,     gi + 1, ri + 1),
                            c101 = SafeAt(lut, bi + 1, gi,     ri + 1),
                            c111 = SafeAt(lut, bi + 1, gi + 1, ri + 1);
                        T tx = b - bi, ty = g - gi, tz = r - ri;
                        const auto nr = __Detail::clerp<T>(c000.R, c010.R, c100.R, c110.R, c001.R, c011.R, c101.R, c111.R, tx, ty, tz);
                        const auto ng = __Detail::clerp<T>(c000.G, c010.G, c100.G, c110.G, c001.G, c011.G, c101.G, c111.G, tx, ty, tz);
                        const auto nb = __Detail::clerp<T>(c000.B, c010.B, c100.B, c110.B, c001.B, c011.B, c101.B, c111.B, tx, ty, tz);
                        return Lut::ColorRgb<T>(nr, ng, nb);
                    }
                }, lut.GetTable());
        }

    public:
        LUT(const std::filesystem::path& cube): lutData(Lut::CubeLut::FromCubeFile(cube))
        {
            dMin = lutData.DomainMin;
            dMax = lutData.DomainMax;
            size = static_cast<float>(lutData.Length());
        }

        void ImgRef(const Image::ImageFile& img) { _ImgRef = &img; }

        ImageSize GetOutputSize() const { return { _ImgRef->Width(), _ImgRef->Height() }; }

        Image::ColorRgba<uint8_t> operator()(const int64_t row, const int64_t col)
        {
            const auto [riR, giR, biR, aiR] = _ImgRef->At<uint8_t>(row, col);
            float bi = biR / 255.f, gi = giR / 255.f, ri = riR / 255.f;

            ri = (ri - dMin.R) / (dMax.R - dMin.R);
            gi = (gi - dMin.G) / (dMax.G - dMin.G);
            bi = (bi - dMin.B) / (dMax.B - dMin.B);

            ri = ri * (size - 1.f);
            gi = gi * (size - 1.f);
            bi = bi * (size - 1.f);

            ri = std::clamp<float>(ri, 0.f, size - 1.f);
            gi = std::clamp<float>(gi, 0.f, size - 1.f);
            bi = std::clamp<float>(bi, 0.f, size - 1.f);

            const auto& nBgr = LookUp(lutData, bi, gi, ri);
            return Image::ColorRgba<uint8_t>(nBgr.R * 255.f, nBgr.G * 255.f, nBgr.B * 255.f, aiR);
        }
    };

    class LinearDodgeColor: ITool<LinearDodgeColor>
    {
	    Image::ColorRgba<uint8_t> color;

        static int16_t ClampAdd(const int16_t x, const int16_t y)
        {
            return std::clamp(x + y, 0, 255);
        }

    public:
        LinearDodgeColor(Image::ColorRgba<uint8_t> color): color(std::move(color)) {}

        void ImgRef(const Image::ImageFile& img) { _ImgRef = &img; }

        ImageSize GetOutputSize() const { return { _ImgRef->Width(), _ImgRef->Height() }; }

        Image::ColorRgba<uint8_t> operator()(const int64_t row, const int64_t col)
        {
            const auto [r0, g0, b0, a0] = _ImgRef->At<uint8_t>(row, col);
            const auto [r1, g1, b1, a1] = color;
            
            return Image::ColorRgba<uint8_t>(ClampAdd(r0, r1), ClampAdd(g0, g1), ClampAdd(b0, b1), ClampAdd(a0, a1));
        }
    };

    class LinearDodgeImage: ITool<LinearDodgeImage>
    {
	    Image::ImageFile image;

        static int16_t ClampAdd(const int16_t x, const int16_t y)
        {
            return std::clamp(x + y, 0, 255);
        }

    public:
        LinearDodgeImage(Image::ImageFile image): image(std::move(image)) {}

        void ImgRef(const Image::ImageFile& img) { _ImgRef = &img; }

        ImageSize GetOutputSize() const { return { _ImgRef->Width(), _ImgRef->Height() }; }

        Image::ColorRgba<uint8_t> operator()(int64_t row, int64_t col)
        {
            const auto [r0, g0, b0, a0] = _ImgRef->At<uint8_t>(row, col);
            const auto [r1, g1, b1, a1] = image.At<int16_t>(row, col);
            
            return Image::ColorRgba<uint8_t>(ClampAdd(r0, r1), ClampAdd(g0, g1), ClampAdd(b0, b1), ClampAdd(a0, a1));
        }
    };

    template <typename T>
    class Sampler
    {
    public:
        const Image::ImageFile* Img;

        T StepRow;
        T StepCol;

        Sampler& operator=(Sampler<T>&& smp) noexcept
        {
            Img = smp.Img;
            StepRow = smp.StepRow;
            StepCol = smp.StepCol;
            return *this;
        }

        Sampler& operator=(const Sampler<T>& smp)
        {
            Img = smp.Img;
            StepRow = smp.StepRow;
            StepCol = smp.StepCol;
            return *this;
        }

        Sampler(const Sampler<T>& smp)
        {
            Img = smp.Img;
            StepRow = smp.StepRow;
            StepCol = smp.StepCol;
        }

        Sampler(Sampler<T>&& smp) noexcept
        {
            Img = smp.Img;
            StepRow = smp.StepRow;
            StepCol = smp.StepCol;
        }

        Sampler(const Image::ImageFile& img)
        {
            Img = &img;
            StepRow = 1.f / (img.Height() - 1);
            StepCol = 1.f / (img.Width() - 1);
        }

        Sampler(const Image::ImageFile& img, T stepRow, T stepCol) : Img(img), StepRow(stepRow), StepCol(stepCol) {}

        const Image::ImageFile& GetImg() const { return *Img; }
        T GetStepRow() const { return StepRow; }
        T GetStepCol() const { return StepCol; }

        Image::ColorRgba<T> operator()(T row, T col) const
        {
            const auto safeAt = [](const Image::ImageFile& img, const int64_t _row, const int64_t _col)
            {
                if (_row >= 0 && _row < img.Height() && _col >= 0 && _col < img.Width()) return img.At<float>(_row, _col);
                return Image::ColorRgba<float>(0, 0, 0, 0);
            };

            const T rx = row * static_cast<T>(Img->Height() - 1);
            const T cx = col * static_cast<T>(Img->Width() - 1);

            const auto r0 = std::round(rx);
            const auto c0 = std::round(cx);

            const Image::ColorRgba<T>
                c00 = safeAt(*Img, static_cast<size_t>(r0), static_cast<size_t>(c0)),
                c10 = safeAt(*Img, static_cast<size_t>(r0) + 1, static_cast<size_t>(c0)),
                c01 = safeAt(*Img, static_cast<size_t>(r0), static_cast<size_t>(c0) + 1),
                c11 = safeAt(*Img, static_cast<size_t>(r0) + 1, static_cast<size_t>(c0) + 1);
            const T tx = rx - r0, ty = cx - c0;

            const auto r = __Detail::blerp<T>(c00.R, c10.R, c01.R, c11.R, tx, ty);
            const auto g = __Detail::blerp<T>(c00.G, c10.G, c01.G, c11.G, tx, ty);
            const auto b = __Detail::blerp<T>(c00.B, c10.B, c01.B, c11.B, tx, ty);
            const auto a = __Detail::blerp<T>(c00.A, c10.A, c01.A, c11.A, tx, ty);

            return Image::ColorRgba<T>(r / 255.f, g / 255.f, b / 255.f, a / 255.f);
        }
    };

    class GenerateNormalTexture: ITool<GenerateNormalTexture>
    {
	    float bias = 50.;
        bool invertR = false;
        bool invertG = false;

        bool init = false;

        Sampler<float> smp = Sampler<float>(Image::ImageFile{});
    public:
        GenerateNormalTexture(const float bias = 50., const bool invertR = false, const bool invertG = false): bias(bias), invertR(invertR), invertG(invertG) {}

        void ImgRef(const Image::ImageFile& img)
        {
            _ImgRef = &img;
            smp = Sampler<float>(*_ImgRef);
        }

        ImageSize GetOutputSize() const { return { _ImgRef->Width(), _ImgRef->Height() }; }

        Image::ColorRgba<uint8_t> operator()(const int64_t row, const int64_t col)
        {
            const int64_t xi = row;
        	const int64_t yi = col;

            const float x = static_cast<float>(xi) / (smp.GetImg().Height() - 1);
            const float y = static_cast<float>(yi) / (smp.GetImg().Width() - 1);

            const float d0 = smp(x,                    y                   ).R;
            const float d1 = smp(x - smp.GetStepRow(), y                   ).R;
            const float d2 = smp(x + smp.GetStepRow(), y                   ).R;
            const float d3 = smp(x,                    y - smp.GetStepCol()).R;
            const float d4 = smp(x,                    y + smp.GetStepCol()).R;

            float dx = ((d2 - d0) + (d0 - d1)) * 0.5f;
            float dy = ((d4 - d0) + (d0 - d3)) * 0.5f;

            dx = dx * (invertR ? -1.f : 1.f);
            dy = dy * (invertG ? -1.f : 1.f);
            float dz = 1.f - ((bias - 0.1f) / 100.f);

            const float len = std::sqrt(dx * dx + dy * dy + dz * dz);

            dx = ((dx / len) * 0.5f + 0.5f) * 255.f;
            dy = ((dy / len) * 0.5f + 0.5f) * 255.f;
            dz = ((dz / len) * 0.5f + 0.5f) * 255.f;

            return Image::ColorRgba<uint8_t>(std::round(dx), std::round(dy), std::round(dz), 255);
        }
    };

    class NormalMapConvert: ITool<NormalMapConvert>
    {
    public:
        enum class Format { RGB = 0, DA = 1 };

    private:
        Format input;
        Format output;

    public:
        NormalMapConvert(const Format& in, const Format& out): input(in), output(out) {}

        void ImgRef(const Image::ImageFile& img) { _ImgRef = &img; }

        ImageSize GetOutputSize() const { return { _ImgRef->Width(), _ImgRef->Height() }; }

        Image::ColorRgba<uint8_t> operator()(const int64_t row, const int64_t col)
        {
            const auto color = _ImgRef->At<uint8_t>(row, col);
            if (input == output) return color;
            if (input == Format::RGB && output == Format::DA)
            {
                const auto [X, Y, Z, A] = color;
                return Image::ColorRgba(Y, Y, Y, X);
            }
            return Image::ColorRgba<uint8_t>(0);
        }
    };
}
