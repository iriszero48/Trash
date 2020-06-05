#include <stdint.h>

#if (_WIN32 || _WIN64)

#include <Windows.h>
#include <bcrypt.h>

#pragma comment(lib, "Bcrypt.lib")

#else

#include <stdio.h>

#endif

void RandomBytes(uint8_t* buf, const uint32_t len)
{
#if (_WIN32 || _WIN64)

	BCRYPT_ALG_HANDLE phAlgorithm;
	BCryptOpenAlgorithmProvider(&phAlgorithm, BCRYPT_RNG_ALGORITHM, NULL, 0);
	BCryptGenRandom(phAlgorithm, buf, len, 0);

#else

	FILE* fd = fopen("/dev/urandom", "rb");
	for (size_t count = 0, n = 0; count < len; count += n)
	{
		n = fread(buf + count, len - count, sizeof(uint8_t), fd);
	}
	fclose(fd);

#endif
}

uint64_t RandomUint64(const uint64_t min, const uint64_t max)
{
	union
	{
		uint64_t uint64;
		uint8_t bytes[sizeof(uint64_t)];
	} result = {0};
	if (min == max) return min;
	const uint64_t dis = max - min + 1;
	RandomBytes(result.bytes, sizeof(uint64_t));
	if (min == 0 && max == UINT64_MAX)
	{
		return result.uint64;
	}
	if ((dis & dis - 1) != 0)
	{
		const uint64_t lim = UINT64_MAX - UINT64_MAX % dis - 1;
		while (result.uint64 > lim)
		{
			RandomBytes(result.bytes, sizeof(uint64_t));
		}
	}
	return result.uint64 % dis + min;
}

#include <stdio.h>

int main(int argc, char* argv[])
{
	uint32_t count[101] = { 0 };
	for (int i = 0; i < 100000; ++i)
	{
		count[RandomUint64(0, 100)]++;
	}
	for (int i = 0; i < 101; ++i)
	{
		printf("%d:%d\n", i, count[i]);
	}
}
