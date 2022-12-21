#include "fucker.h"

#include <cassert>
#include <stdexcept>
#include <sstream>

#include "cuda_runtime.h"
#include "device_launch_parameters.h"

enum class Round
{
    F,
    G,
    H,
    I
};

#define _Error(...)\
	std::runtime_error((std::ostringstream{} << "[" << __FILE__ << ":" << __LINE__ << "] [" << __FUNCTION__ << "] " << __VA_ARGS__).str())


template <typename T>
class CudaArray
{
    T* data = nullptr;

public:
    explicit CudaArray(const size_t size)
    {
        CudaMalloc((T**)&data, sizeof(T) * size);
    }

    explicit CudaArray(const T* data, const size_t size)
    {
        CudaMalloc(&this->data, size);
        const auto err = cudaMemcpy(this->data, data, size, cudaMemcpyHostToDevice);
        if (err != cudaSuccess)
            throw _Error("memcpy: " << cudaGetErrorString(err));
    }

    [[nodiscard]] T* Get() const
    {
        return data;
    }

    ~CudaArray()
    {
        cudaFree(data);
    }

private:
    void CudaMalloc(T** devPtr, const size_t size) const
    {
        const auto err = cudaMalloc(devPtr, size);
        if (err != cudaSuccess)
            throw _Error("malloc: " << cudaGetErrorString(err));
    }
};


__device__ constexpr std::uint32_t F(const std::uint32_t x, const std::uint32_t y, const std::uint32_t z)
{
    return z ^ x & (y ^ z);
}

__device__ constexpr std::uint32_t G(const std::uint32_t x, const std::uint32_t y, const std::uint32_t z)
{
    return y ^ z & (x ^ y);
}

__device__ constexpr std::uint32_t H(const std::uint32_t x, const std::uint32_t y, const std::uint32_t z)
{
    return x ^ y ^ z;
}

__device__ constexpr std::uint32_t I(const std::uint32_t x, const std::uint32_t y, const std::uint32_t z)
{
    return y ^ (x | ~z);
}

template <Round Func>
__device__ void Step(std::uint32_t& a, const std::uint32_t b, const std::uint32_t c, const std::uint32_t d, const std::uint32_t x, const std::uint32_t t, const std::uint32_t s)
{
    if constexpr (Func == Round::F)
        a += F(b, c, d) + x + t;
    if constexpr (Func == Round::G)
        a += G(b, c, d) + x + t;
    if constexpr (Func == Round::H)
        a += H(b, c, d) + x + t;
    if constexpr (Func == Round::I)
        a += I(b, c, d) + x + t;
    a = a << s | (a >> (32u - s));
    a += b;
}

__device__ std::uint32_t Get(const std::uint8_t* buf, const std::uint64_t index)
{
	return *(std::uint32_t*)&buf[index * 4];
}

__device__ void FuckingMd5ARound(const uint8_t* key, const size_t keySize, char res[32])
{
    uint8_t buf[64]{ 0 };
    for (size_t i = 0; i < keySize; ++i) buf[i] = key[i];

    buf[keySize] = 0x80;
    *reinterpret_cast<uint64_t*>(&buf[64 - 8]) = keySize * 8;

    uint32_t a = 0x67452301, b = 0xefcdab89, c = 0x98badcfe, d = 0x10325476;

    const uint32_t savedA = a;
    const uint32_t savedB = b;
    const uint32_t savedC = c;
    const uint32_t savedD = d;

    Step<Round::F>(a, b, c, d, Get(buf, 0), 0xd76aa478u, 7);
    Step<Round::F>(d, a, b, c, Get(buf, 1), 0xe8c7b756u, 12);
    Step<Round::F>(c, d, a, b, Get(buf, 2), 0x242070dbu, 17);
    Step<Round::F>(b, c, d, a, Get(buf, 3), 0xc1bdceeeu, 22);
    Step<Round::F>(a, b, c, d, Get(buf, 4), 0xf57c0fafu, 7);
    Step<Round::F>(d, a, b, c, Get(buf, 5), 0x4787c62au, 12);
    Step<Round::F>(c, d, a, b, Get(buf, 6), 0xa8304613u, 17);
    Step<Round::F>(b, c, d, a, Get(buf, 7), 0xfd469501u, 22);
    Step<Round::F>(a, b, c, d, Get(buf, 8), 0x698098d8u, 7);
    Step<Round::F>(d, a, b, c, Get(buf, 9), 0x8b44f7afu, 12);
    Step<Round::F>(c, d, a, b, Get(buf, 10), 0xffff5bb1u, 17);
    Step<Round::F>(b, c, d, a, Get(buf, 11), 0x895cd7beu, 22);
    Step<Round::F>(a, b, c, d, Get(buf, 12), 0x6b901122u, 7);
    Step<Round::F>(d, a, b, c, Get(buf, 13), 0xfd987193u, 12);
    Step<Round::F>(c, d, a, b, Get(buf, 14), 0xa679438eu, 17);
    Step<Round::F>(b, c, d, a, Get(buf, 15), 0x49b40821u, 22);

    Step<Round::G>(a, b, c, d, Get(buf, 1), 0xf61e2562u, 5);
    Step<Round::G>(d, a, b, c, Get(buf, 6), 0xc040b340u, 9);
    Step<Round::G>(c, d, a, b, Get(buf, 11), 0x265e5a51u, 14);
    Step<Round::G>(b, c, d, a, Get(buf, 0), 0xe9b6c7aau, 20);
    Step<Round::G>(a, b, c, d, Get(buf, 5), 0xd62f105du, 5);
    Step<Round::G>(d, a, b, c, Get(buf, 10), 0x02441453u, 9);
    Step<Round::G>(c, d, a, b, Get(buf, 15), 0xd8a1e681u, 14);
    Step<Round::G>(b, c, d, a, Get(buf, 4), 0xe7d3fbc8u, 20);
    Step<Round::G>(a, b, c, d, Get(buf, 9), 0x21e1cde6u, 5);
    Step<Round::G>(d, a, b, c, Get(buf, 14), 0xc33707d6u, 9);
    Step<Round::G>(c, d, a, b, Get(buf, 3), 0xf4d50d87u, 14);
    Step<Round::G>(b, c, d, a, Get(buf, 8), 0x455a14edu, 20);
    Step<Round::G>(a, b, c, d, Get(buf, 13), 0xa9e3e905u, 5);
    Step<Round::G>(d, a, b, c, Get(buf, 2), 0xfcefa3f8u, 9);
    Step<Round::G>(c, d, a, b, Get(buf, 7), 0x676f02d9u, 14);
    Step<Round::G>(b, c, d, a, Get(buf, 12), 0x8d2a4c8au, 20);

    Step<Round::H>(a, b, c, d, Get(buf, 5), 0xfffa3942u, 4);
    Step<Round::H>(d, a, b, c, Get(buf, 8), 0x8771f681u, 11);
    Step<Round::H>(c, d, a, b, Get(buf, 11), 0x6d9d6122u, 16);
    Step<Round::H>(b, c, d, a, Get(buf, 14), 0xfde5380cu, 23);
    Step<Round::H>(a, b, c, d, Get(buf, 1), 0xa4beea44u, 4);
    Step<Round::H>(d, a, b, c, Get(buf, 4), 0x4bdecfa9u, 11);
    Step<Round::H>(c, d, a, b, Get(buf, 7), 0xf6bb4b60u, 16);
    Step<Round::H>(b, c, d, a, Get(buf, 10), 0xbebfbc70u, 23);
    Step<Round::H>(a, b, c, d, Get(buf, 13), 0x289b7ec6u, 4);
    Step<Round::H>(d, a, b, c, Get(buf, 0), 0xeaa127fau, 11);
    Step<Round::H>(c, d, a, b, Get(buf, 3), 0xd4ef3085u, 16);
    Step<Round::H>(b, c, d, a, Get(buf, 6), 0x04881d05u, 23);
    Step<Round::H>(a, b, c, d, Get(buf, 9), 0xd9d4d039u, 4);
    Step<Round::H>(d, a, b, c, Get(buf, 12), 0xe6db99e5u, 11);
    Step<Round::H>(c, d, a, b, Get(buf, 15), 0x1fa27cf8u, 16);
    Step<Round::H>(b, c, d, a, Get(buf, 2), 0xc4ac5665u, 23);

    Step<Round::I>(a, b, c, d, Get(buf, 0), 0xf4292244u, 6);
    Step<Round::I>(d, a, b, c, Get(buf, 7), 0x432aff97u, 10);
    Step<Round::I>(c, d, a, b, Get(buf, 14), 0xab9423a7u, 15);
    Step<Round::I>(b, c, d, a, Get(buf, 5), 0xfc93a039u, 21);
    Step<Round::I>(a, b, c, d, Get(buf, 12), 0x655b59c3u, 6);
    Step<Round::I>(d, a, b, c, Get(buf, 3), 0x8f0ccc92u, 10);
    Step<Round::I>(c, d, a, b, Get(buf, 10), 0xffeff47du, 15);
    Step<Round::I>(b, c, d, a, Get(buf, 1), 0x85845dd1u, 21);
    Step<Round::I>(a, b, c, d, Get(buf, 8), 0x6fa87e4fu, 6);
    Step<Round::I>(d, a, b, c, Get(buf, 15), 0xfe2ce6e0u, 10);
    Step<Round::I>(c, d, a, b, Get(buf, 6), 0xa3014314u, 15);
    Step<Round::I>(b, c, d, a, Get(buf, 13), 0x4e0811a1u, 21);
    Step<Round::I>(a, b, c, d, Get(buf, 4), 0xf7537e82u, 6);
    Step<Round::I>(d, a, b, c, Get(buf, 11), 0xbd3af235u, 10);
    Step<Round::I>(c, d, a, b, Get(buf, 2), 0x2ad7d2bbu, 15);
    Step<Round::I>(b, c, d, a, Get(buf, 9), 0xeb86d391u, 21);

    a += savedA;
    b += savedB;
    c += savedC;
    d += savedD;

    auto ptr = res;

#define ToHex(x) ((x) > 9 ? (x) + 55 : (x) + 48)
#define VToHex(v) \
    *ptr++ = ToHex(static_cast<uint8_t>(v) >> 4);\
    *ptr++ = ToHex(static_cast<uint8_t>(v) % 16)
#define Uint32ToHex(u32)\
	VToHex(((uint8_t*)&u32)[0]);\
    VToHex(((uint8_t*)&u32)[1]);\
    VToHex(((uint8_t*)&u32)[2]);\
    VToHex(((uint8_t*)&u32)[3])
    Uint32ToHex(a);
    Uint32ToHex(b);
    Uint32ToHex(c);
    Uint32ToHex(d);
}

__device__ void AddBaseN(uint8_t* c, const uint8_t* const a, const uint8_t* const b, const size_t n, const uint8_t base)
{
    int16_t carry = 0;
    for (int64_t i = n - 1; i >= 0; i--)
    {
        int16_t curr = carry + a[i] + b[i];
        carry = curr / base;
        curr %= base;
        c[i] = curr;
    }
    if (carry > 0)
        assert(("AddBaseN", false));
}

__device__ void AddUint64BaseN(uint8_t* c, const uint8_t* const a, uint64_t b, const size_t n, const uint8_t base, uint8_t* nb)
{
    for (int64_t i = n - 1; i >= 0; --i)
    {
        nb[i] = b % base;
        b /= base;
    }
    if (b > 0)
        assert(("AddUint64BaseN", false));

    AddBaseN(c, a, nb, n, base);
}

__global__ void FuckingCudaCall(
    const char* prefix, const size_t prefixSize,
    const char* suffix, const size_t suffixSize,
    const size_t dynamic,
    const char* alphabet, const size_t alphabetSize,
    const size_t offset,
    char* out,
    uint8_t* aBuf, uint8_t* bBuf, uint8_t* cBuf, const uint64_t maxId)
{
    const uint64_t id = blockIdx.x * blockDim.x + threadIdx.x;
    if (id >= maxId) return;

    const auto keySize = prefixSize + suffixSize + dynamic;
    const auto elemSize = keySize + 1 + 32 + 1;

    uint8_t* a = aBuf + id * dynamic;
    for (size_t i = 0; i < dynamic; ++i) a[i] = 0;

    const uint64_t b = offset + id;

    uint8_t* c = cBuf + id * dynamic;

    AddUint64BaseN(c, a, b, dynamic, alphabetSize, bBuf + dynamic * id);

    char* key = out + elemSize * id;
    for (size_t i = 0; i < prefixSize; ++i) key[i] = prefix[i];
    for (size_t i = 0; i < dynamic; ++i) key[prefixSize + i] = alphabet[c[i]];
    for (size_t i = 0; i < suffixSize; ++i) key[prefixSize + dynamic + i] = suffix[i];
    key[keySize] = ' ';

    FuckingMd5ARound((const uint8_t*)key, keySize, out + id * elemSize + keySize + 1);

    key[elemSize - 1] = '\n';
}

CudaRes FuckingCall(
    const StringView& prefix, const StringView& suffix,
    const size_t dynamic,
    const StringView& alphabet,
    const size_t offset, const size_t num)
{
    CudaArray<char> prefixCuda(prefix.Data, prefix.Size);
    CudaArray<char> suffixCuda(suffix.Data, suffix.Size);
    CudaArray<char> alphabetCuda(alphabet.Data, alphabet.Size);

    CudaArray<uint8_t> aBuf(num * dynamic);
    CudaArray<uint8_t> bBuf(num * dynamic);
    CudaArray<uint8_t> cBuf(num * dynamic);

    const auto outSize = num * (prefix.Size + suffix.Size + dynamic + 1 + 32 + 1);

	char* outBuf = nullptr;
    auto err = cudaMallocManaged(&outBuf, outSize);
    if (err != cudaSuccess) throw _Error("malloc managed: " << cudaGetErrorString(err));

    FuckingCudaCall<<<num / 200 + (num % 200 == 0 ? 0 : 1), 200>>>(
        prefixCuda.Get(), prefix.Size, 
        suffixCuda.Get(), suffix.Size,
        dynamic,
        alphabetCuda.Get(), alphabet.Size,
        offset, outBuf,
        aBuf.Get(), bBuf.Get(), cBuf.Get(), num);

	err = cudaGetLastError();
    if (err != cudaSuccess) throw _Error("FuckingCudaCall: " << cudaGetErrorString(err));

    err = cudaDeviceSynchronize();
    if (err != cudaSuccess) throw _Error("Synchronize: " << cudaGetErrorString(err));

    return { outBuf, outSize };
}

void FuckingFree(CudaRes& sv)
{
    const auto err = cudaFree(sv.Data);
    if (err != cudaSuccess) throw _Error("free: " << cudaGetErrorString(err));
    sv = {};
}
