//Library: ZipUtils
#include "unzip.h"
#include <cstdint>
#include <cstdio>
#include <string>
#include <tchar.h>
#include <sys/stat.h>
#include <thread>
#include <valarray>
#include <regex>

#ifdef VS_VERSION_INFO
#define _CRT_SECURE_NO_WARNINGS
#pragma warning(disable : 4996)
#endif

int main(int argc, char* argv[])
{
	auto path = argv[1];
	struct stat statbuf;
	stat(path, &statbuf);
	uint64_t fileSize = statbuf.st_size;
	auto fp = fopen(path, "rb");
	auto buf1 = new uint8_t[fileSize * 2];
	auto buf2 = new uint8_t[fileSize * 2];
	fread(buf1, sizeof uint8_t, fileSize, fp);
	fclose(fp);
	for (auto i = 0; i < 100000 - 1; ++i)
	{
		auto hz = OpenZip(buf1, fileSize, "");
		ZIPENTRY ze;
		GetZipItem(hz, -1, &ze);
		for (auto zi = 0; zi < 1; zi++)
		{
			GetZipItem(hz, zi, &ze);
			if (ze.name[0] == '1'&&ze.name[1] == '.')
			{
				UnzipItem(hz, zi, _T("1.zip"));
				exit(EXIT_SUCCESS);
			}
			UnzipItem(hz, zi, buf2, ze.unc_size);
			memcpy(buf1, buf2, ze.unc_size);
			fileSize = ze.unc_size;
		}
		CloseZip(hz);
	}
}
