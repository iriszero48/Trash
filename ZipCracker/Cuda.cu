#include "Cuda.h"

#include <stdexcept>

#include "cuda_runtime.h"
#include "device_launch_parameters.h"



__device__ void AddBaseN(char* c, const char* const a, const char* const b, const uint64_t n, const uint64_t base)
{
    int carry = 0;
    for (int i = n - 1; i >= 0; i--)
    {
        int curr = carry + a[i] + b[i];
        carry = curr / base;
        curr %= base;
        c[i] = curr;
    }
    //if (carry > 0); //Impossible!
}

__device__ void AddUint64BaseN(char* c, const char* const a, uint64_t b, const uint64_t n, const uint64_t base)
{
    char* nb;
    cudaMalloc((void**)&nb, n * sizeof(char));
    for (int i = n - 1; i >= 0; --i)
    {
        nb[i] = b % base;
        b /= base;
    }
    nb[0] = b;
    AddBaseN(c, a, nb, n, base);
    cudaFree(nb);
}

__device__ uint32_t crc32Cuda(const uint32_t crc, const uint8_t val, const uint32_t* crc_32_tab)
{
    return crc_32_tab[static_cast<uint8_t>(crc) ^ val] ^ crc >> 8;
}

constexpr uint64_t Rate = 1;

__global__ void FuckZipCrc(
    uint8_t* res,
    const uint8_t* fileHead,
    const char* passwordIndex,
    const uint64_t passwordLen,
    const char* alphabet,
    const uint64_t alphabetLen,
    const uint64_t maxId,
    const uint8_t* mult_tab,
    const uint32_t* crc_32_tab)
{
    const uint64_t id = blockIdx.x * blockDim.x + threadIdx.x;
    if (id >= maxId) return;
    for (int loop = 0; loop < Rate; ++loop)
    {
        const auto readId = id * Rate + loop;
        //const auto readId = id;
        char* pw;
        cudaMalloc((void**)&pw, passwordLen * sizeof(char));
        AddUint64BaseN(pw, passwordIndex, readId, passwordLen, alphabetLen);
        for (int i = 0; i < passwordLen; ++i)
        {
            pw[i] = alphabet[pw[i]];
        }
        uint32_t key0 = 0x12345678UL;
        uint32_t key1 = 0x23456789UL;
        uint32_t key2 = 0x34567890UL;
        for (int i = 0; i < passwordLen; ++i)
        {
            key0 = crc32Cuda(key0, pw[i], crc_32_tab);
            key1 = (key1 + static_cast<uint8_t>(key0)) * 134775813 + 1;
            key2 = crc32Cuda(key2, key1 >> 24, crc_32_tab);
        }
        for (int i = 0; i < 11; ++i)
        {
            const uint8_t preTarget = fileHead[i] ^ mult_tab[static_cast<uint16_t>(key2) >> 2];
            key0 = crc32Cuda(key0, preTarget, crc_32_tab);
            key1 = (key1 + static_cast<uint8_t>(key0)) * 134775813 + 1;
            key2 = crc32Cuda(key2, key1 >> 24, crc_32_tab);
        }
        const uint8_t target = fileHead[11] ^ mult_tab[static_cast<uint16_t>(key2) >> 2];
        res[readId] = target == fileHead[12];
        cudaFree(pw);
    }
}

__constant__ uint8_t mult_tab[16384];
__constant__ uint32_t crc_32_tab[256];

void FuckZipCrcCuda(uint8_t* flags, const uint8_t* fileHead, const std::string& password, const std::string& alphabet, const uint64_t chunkSize)
{
    cudaError_t error;

    uint8_t* resCuda;
    uint8_t* fileHeadCuda;

    if (static bool init = false; !init)
    {
        uint8_t tab[16384];
        for (auto t = 0; t < 16384; t++) tab[t] = ((t * 4 + 3) * (t * 4 + 2) >> 8) & 0xff;
        error = cudaMemcpyToSymbol(mult_tab, tab, sizeof(tab));
        if (error != cudaSuccess) throw std::runtime_error(std::string("FuckZipCrc: tab: cudaMemcpy: ") + cudaGetErrorString(error));

        uint32_t crcTab[256] = { 0 };
        for (uint32_t i = 0; i < 256; ++i)
        {
            auto checksum = i;
            for (auto j = 0; j < 8; ++j) checksum = (checksum >> 1) ^ (checksum & 0x1 ? 0xEDB88320 : 0);
            crcTab[i] = checksum;
        }
        error = cudaMemcpyToSymbol(crc_32_tab, crcTab, sizeof(crcTab));
        if (error != cudaSuccess) throw std::runtime_error(std::string("FuckZipCrc: crcTab: cudaMemcpy: ") + cudaGetErrorString(error));

        error = cudaMalloc(reinterpret_cast<void**>(&resCuda), chunkSize * sizeof(uint8_t));
        if (error != cudaSuccess) throw std::runtime_error(std::string("FuckZipCrc: resCuda: cudaMalloc: ") + cudaGetErrorString(error));

        error = cudaMalloc(reinterpret_cast<void**>(&fileHeadCuda), 14 * sizeof(uint8_t));
        if (error != cudaSuccess) throw std::runtime_error(std::string("FuckZipCrc: fileHeadCuda: cudaMalloc: ") + cudaGetErrorString(error));
        error = cudaMemcpy(fileHeadCuda, fileHead, 14 * sizeof(uint8_t), cudaMemcpyHostToDevice);
        if (error != cudaSuccess) throw std::runtime_error(std::string("FuckZipCrc: fileHeadCuda: cudaMemcpy: ") + cudaGetErrorString(error));

        init = true;
    }

    auto pwIndex = password;
    for (auto i = 0; i < password.length(); ++i) pwIndex[i] = alphabet.find(pwIndex[i]);
    char* pwIndexCuda;
    error = cudaMalloc(reinterpret_cast<void**>(&pwIndexCuda), password.length() * sizeof(char));
    if (error != cudaSuccess) throw std::runtime_error(std::string("FuckZipCrc: pwIndexCuda: cudaMalloc: ") + cudaGetErrorString(error));
    error = cudaMemcpy(pwIndexCuda, pwIndex.data(), pwIndex.length() * sizeof(char), cudaMemcpyHostToDevice);
    if (error != cudaSuccess) throw std::runtime_error(std::string("FuckZipCrc: pwIndexCuda: cudaMemcpy: ") + cudaGetErrorString(error));

    char* alphabetCuda;
    error = cudaMalloc(reinterpret_cast<void**>(&alphabetCuda), alphabet.length() * sizeof(char));
    if (error != cudaSuccess) throw std::runtime_error(std::string("FuckZipCrc: alphabetCuda: cudaMalloc: ") + cudaGetErrorString(error));
    error = cudaMemcpy(alphabetCuda, alphabet.data(), alphabet.length() * sizeof(char), cudaMemcpyHostToDevice);
    if (error != cudaSuccess) throw std::runtime_error(std::string("FuckZipCrc: alphabetCuda: cudaMemcpy: ") + cudaGetErrorString(error));

    uint8_t* a;
    uint32_t* b;
    cudaGetSymbolAddress((void**)&a, mult_tab);
    cudaGetSymbolAddress((void**)&b, crc_32_tab);
	
    FuckZipCrc<<<chunkSize / Rate / 1000 + 1, 1000>>>(
        resCuda,
        fileHeadCuda,
        pwIndexCuda, pwIndex.length(),
        alphabetCuda, alphabet.length(),
        chunkSize / Rate,
        a, b);
    error = cudaGetLastError();
    if (error != cudaSuccess) throw std::runtime_error(std::string("FuckZipCrc: FuckZipCrc: ") + cudaGetErrorString(error));

    error = cudaDeviceSynchronize();
    if (error != cudaSuccess) throw std::runtime_error(std::string("FuckZipCrc: cudaDeviceSynchronize: ") + cudaGetErrorString(error));
	
    error = cudaMemcpy(flags, resCuda, chunkSize * sizeof(uint8_t), cudaMemcpyDeviceToHost);
    if (error != cudaSuccess) throw std::runtime_error(std::string("FuckZipCrc: res: cudaMemcpy: ") + cudaGetErrorString(error));
    
    //cudaFree(resCuda);
    //cudaFree(fileHeadCuda);
    cudaFree(pwIndexCuda);
    cudaFree(alphabetCuda);

    //cudaFree(tab);
    //cudaFree(crc_32_tab);
}
