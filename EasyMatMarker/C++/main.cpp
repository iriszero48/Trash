#include <iostream>
#include <array>
#include <chrono>
#include <numeric>
#include <random>

// #define USE_I64

#ifndef MAT_SIZE
#define MAT_SIZE 128
#endif

constexpr auto GetFloat64TImpl()
{
    if constexpr (sizeof(long double) == 8)
    {
        return (long double)0;
    }
    else if constexpr (sizeof(double) == 8)
    {
        return (double)0;
    }
    else if constexpr (sizeof(float) == 8)
    {
        return (float)0;
    }
    else
    {
        return 0;
    }
}

constexpr auto GetFloat64T()
{
    using T = decltype(GetFloat64TImpl());
    static_assert(std::is_floating_point_v<T> && sizeof(T) == 8);

    return (T)0;
}

using Float64T = decltype(GetFloat64T());

struct Mat
{
#ifdef USE_I64
    using T = int64_t;
#else
    using T = Float64T;
#endif

    static constexpr std::size_t LineSize = MAT_SIZE;
    static constexpr std::size_t Row = LineSize;
    static constexpr std::size_t Col = LineSize;

    std::array<T, Row * Col> Data{};

    constexpr Mat()
    {
        static_assert(sizeof(T) == 8);
    }

    constexpr static Mat One()
    {
        Mat ret;
        ret.Data.fill(1);
        return ret;
    }

    constexpr void Set(std::size_t row, std::size_t col, T val)
    {
        Data[row * Col + col] = val;
    }

    constexpr T At(std::size_t row, std::size_t col) const
    {
        return Data[row * Col + col];
    }

    // c = a * b
    static constexpr void Mul(const Mat& a, const Mat& b, Mat& c)
    {
	    for (std::size_t row = 0; row < a.Row; ++row)
	    {
		    for (std::size_t col = 0; col < b.Col; ++col)
		    {
                T v = 0;
			    for (std::size_t i = 0; i < a.Col; ++i)
			    {
                    v += a.At(row, i) * b.At(i, col);
			    }
                c.Set(row, col, v);
		    }
	    }
    }

    constexpr friend Mat& operator+=(Mat& a, T b)
    {
	    for (std::size_t i = 0; i < Row * Col; ++i)
	    {
            a.Data[i] += b;
	    }

        return a;
    }

    constexpr friend Mat& operator/=(Mat& a, T b)
    {
        for (std::size_t i = 0; i < Row * Col; ++i)
        {
            a.Data[i] /= b;
        }

        return a;
    }
};


int main(int, char**){
    Mat a{};
    std::random_device r{};
    std::mt19937_64 re(r());

#ifdef USE_I64
    std::uniform_int_distribution<Mat::T> dist(0);
#else
    std::uniform_real_distribution<Mat::T> dist(0., 1.);
#endif

    for (std::size_t i = 0; i < a.Row * a.Col; ++i)
    {
        // a.Data[i] = (Mat::T)(i + 1) / (a.Row * a.Col);
        a.Data[i] = i == (a.Row * a.Col - 1) ?
#ifdef USE_I64
            std::numeric_limits<Mat::T>::max()
#else
            1.
#endif
            : dist(re);
    }

    auto b = Mat::One();
    Mat c{};
    Mat::T sum = 0;

    const auto tp1 = std::chrono::high_resolution_clock::now();
    for (int i = 0; i < 1000; ++i)
    {
        b += i;
        Mat::Mul(a, b, c);

        b += i + 1;
        Mat::Mul(c, b, a);

        a /= c.At(c.Row - 1, c.Col - 1);
    }

    for (std::size_t i = 0; i < a.Row * a.Col; ++i)
    {
        sum += a.Data[i];
    }
    const auto tp2 = std::chrono::high_resolution_clock::now();

    std::cout << std::chrono::duration_cast<std::chrono::duration<double, std::milli>>(tp2 - tp1).count() << " " << sum << std::endl;
}
