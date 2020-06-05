#include <cstdint>

#if (_WIN32 || _WIN64)

#include <Windows.h>
#include <bcrypt.h>

#pragma comment(lib, "Bcrypt.lib")

#else

#include <stdio.h>

#endif

class RandomNumber
{
public:
	RandomNumber()
	{
		Initialize();
	}
	
	~RandomNumber()
	{
#if (_WIN32 || _WIN64)
		BCryptCloseAlgorithmProvider(phAlgorithm, 0);
#else
		fclose(fd);
#endif
	}

	RandomNumber(const RandomNumber& randomNumber) = default;

	RandomNumber(RandomNumber&& randomNumber) noexcept
	{
#if (_WIN32 || _WIN64)
		BCryptCloseAlgorithmProvider(randomNumber.phAlgorithm, 0);
#else
		fclose(randomNumber.fd);
#endif
		Initialize();
	}

	RandomNumber& operator=(const RandomNumber& randomNumber) = default;

	RandomNumber& operator=(RandomNumber&& randomNumber) noexcept
	{
	#if (_WIN32 || _WIN64)
			BCryptCloseAlgorithmProvider(randomNumber.phAlgorithm, 0);
	#else
			fclose(randomNumber.fd);
	#endif
		Initialize();
		return *this;
	}

	void RandomBytes(uint8_t* buf, const uint32_t len) const
	{
#if (_WIN32 || _WIN64)
		BCryptGenRandom(phAlgorithm, buf, len, 0);
#else
		for (size_t count = 0, n = 0; count < len; count += n)
		{
			n = fread(buf + count, len - count, sizeof(uint8_t), fd);
		}
#endif
	}

	uint64_t RandomUint64(const uint64_t min, const uint64_t max) const
	{
		union
		{
			uint64_t uint64;
			uint8_t bytes[sizeof(uint64_t)];
		} result = { 0 };
		if (min == max) return min;
		const auto dis = max - min + 1;
		RandomBytes(result.bytes, sizeof(uint64_t));
		if (min == 0 && max == UINT64_MAX)
		{
			return result.uint64;
		}
		if ((dis & dis - 1) != 0)
		{
			const auto lim = UINT64_MAX - UINT64_MAX % dis - 1;
			while (result.uint64 > lim)
			{
				RandomBytes(result.bytes, sizeof(uint64_t));
			}
		}
		return result.uint64 % dis + min;
	}
	
private:
#if (_WIN32 || _WIN64)
	BCRYPT_ALG_HANDLE phAlgorithm = nullptr;
#else
	FILE* fd;
#endif

	void Initialize()
	{
#if (_WIN32 || _WIN64)
		BCryptOpenAlgorithmProvider(&phAlgorithm, BCRYPT_RNG_ALGORITHM, nullptr, 0);
#else
		fd = fopen("/dev/urandom", "rb");
#endif
	}
};

#include <cstdio>

int main(int argc, char* argv[])
{
	RandomNumber rn{};
	uint32_t count[101] = {0};
	for (int i = 0; i < 100000; ++i)
	{
		count[rn.RandomUint64(0, 100)]++;
	}
	for (int i = 0; i < 101; ++i)
	{
		printf("%d:%d\n", i, count[i]);
	}
}
