#pragma once

#include <string>
#include <vector>

struct StringView
{
	const char *Data = nullptr;
	size_t Size = 0;
};

struct CudaRes
{
	char *Data = nullptr;
	size_t Size = 0;
};

CudaRes FuckingCall(const StringView &prefix, const StringView &suffix, const size_t dynamic, const StringView &alphabet, size_t offset, size_t num);

void FuckingFree(CudaRes &sv);