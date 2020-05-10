#include <cstdint>
#include <cinttypes>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <string>
#include <regex>
#include <map>
#include <tuple>
#include <fstream>
#include <deque>
#include <filesystem>

#include <omp.h>

#include <boost/serialization/map.hpp>
#include <boost/archive/binary_iarchive.hpp>
#include <boost/archive/binary_oarchive.hpp>
#include <boost/locale.hpp>
#include <boost/filesystem.hpp>

#if (defined _WIN32 || defined _WIN64)

#define StringFormatChar "%hs"
#define LogFunc wprintf
#define LogErrFunc fwprintf
#define FormatString L
#define NativeStringType std::wstring
#define FileOpenFunc _wfopen
#define OmpParallel __pragma(omp parallel)
#define StartWithFunc data._Starts_with(keyword)
#define PathPerfix L"\\\\?\\" +

#else

#define StringFormatChar "%s"
#define LogFunc printf
#define LogErrFunc fprintf
#define FormatString
#define NativeStringType std::string
#define FileOpenFunc fopen
#define OmpParallel _Pragma("omp parallel")
#define StartWithFunc data.find(keyword) == 0
#define PathPerfix

#endif

#define _ToString(x) #x
#define ToString(x) _ToString(x)

#define Line ToString(__LINE__)

#define Log(...) LogFunc(FormatString"<%s,<" StringFormatChar ",%" PRIu64 "," StringFormatChar ">>\n", __VA_ARGS__)
#define LogErr(...) LogErrFunc(stderr, FormatString"    %s(" Line "): " StringFormatChar "\n", __VA_ARGS__)

#define FileOpen(path, mode) FileOpenFunc(path, FormatString"" mode)

using K = std::string;
using V = std::tuple<std::string, uint64_t, std::string>;
using KV = std::pair<K, V>;
using Database = std::map<K, V>;

static Database FileMd5Database{};

class ParameterError final : public std::exception
{
public:
	[[nodiscard]] char const* what() const noexcept override
	{
		return " " __FILE__ ":" Line " " __DATE__ " " __TIME__
			R"(
Parameter Error
Usage: databaseFilePath command [options]
	build deviceName rootPath [logOutput[default:true|false]]
	query queryMethod[contain|startWith|regex] queryData[path|md5|size|fileModificationTime] keyword [limit(default:100))]
	concat sourceDatabaseFile1 sourceDatabaseFile2 [sourceDatabaseFile...]
)";
	}
};

#pragma region MD5

// see https://github.com/php/php-src/blob/master/ext/standard/md5.c

#define F(x, y, z)			((z) ^ ((x) & ((y) ^ (z))))
#define G(x, y, z)			((y) ^ ((z) & ((x) ^ (y))))
#define H(x, y, z)			((x) ^ (y) ^ (z))
#define I(x, y, z)			((y) ^ ((x) | ~(z)))

#define STEP(f, a, b, c, d, x, t, s) \
	(a) += f((b), (c), (d)) + (x) + (t); \
	(a) = (((a) << (s)) | (((a) & 0xffffffff) >> (32 - (s)))); \
	(a) += (b);

# define SET(n) (*(uint32_t*)&ptr[(n) * 4])
# define GET(n) SET(n)

typedef struct
{
	uint32_t lo, hi;
	uint32_t a, b, c, d;
	unsigned char buffer[64];
	uint32_t block[16];
} Md5Ctx;

inline void Md5Init(Md5Ctx* ctx)
{
	ctx->a = 0x67452301;
	ctx->b = 0xefcdab89;
	ctx->c = 0x98badcfe;
	ctx->d = 0x10325476;

	ctx->lo = 0;
	ctx->hi = 0;
}

static const void* Md5Body(Md5Ctx* ctx, const void* data, size_t size)
{
	const unsigned char* ptr;
	uint32_t a, b, c, d;
	uint32_t savedA, savedB, savedC, savedD;

	ptr = static_cast<const unsigned char*>(data);

	a = ctx->a;
	b = ctx->b;
	c = ctx->c;
	d = ctx->d;

	do
	{
		savedA = a;
		savedB = b;
		savedC = c;
		savedD = d;

		STEP(F, a, b, c, d, SET(0), 0xd76aa478, 7)
		STEP(F, d, a, b, c, SET(1), 0xe8c7b756, 12)
		STEP(F, c, d, a, b, SET(2), 0x242070db, 17)
		STEP(F, b, c, d, a, SET(3), 0xc1bdceee, 22)
		STEP(F, a, b, c, d, SET(4), 0xf57c0faf, 7)
		STEP(F, d, a, b, c, SET(5), 0x4787c62a, 12)
		STEP(F, c, d, a, b, SET(6), 0xa8304613, 17)
		STEP(F, b, c, d, a, SET(7), 0xfd469501, 22)
		STEP(F, a, b, c, d, SET(8), 0x698098d8, 7)
		STEP(F, d, a, b, c, SET(9), 0x8b44f7af, 12)
		STEP(F, c, d, a, b, SET(10), 0xffff5bb1, 17)
		STEP(F, b, c, d, a, SET(11), 0x895cd7be, 22)
		STEP(F, a, b, c, d, SET(12), 0x6b901122, 7)
		STEP(F, d, a, b, c, SET(13), 0xfd987193, 12)
		STEP(F, c, d, a, b, SET(14), 0xa679438e, 17)
		STEP(F, b, c, d, a, SET(15), 0x49b40821, 22)

		STEP(G, a, b, c, d, GET(1), 0xf61e2562, 5)
		STEP(G, d, a, b, c, GET(6), 0xc040b340, 9)
		STEP(G, c, d, a, b, GET(11), 0x265e5a51, 14)
		STEP(G, b, c, d, a, GET(0), 0xe9b6c7aa, 20)
		STEP(G, a, b, c, d, GET(5), 0xd62f105d, 5)
		STEP(G, d, a, b, c, GET(10), 0x02441453, 9)
		STEP(G, c, d, a, b, GET(15), 0xd8a1e681, 14)
		STEP(G, b, c, d, a, GET(4), 0xe7d3fbc8, 20)
		STEP(G, a, b, c, d, GET(9), 0x21e1cde6, 5)
		STEP(G, d, a, b, c, GET(14), 0xc33707d6, 9)
		STEP(G, c, d, a, b, GET(3), 0xf4d50d87, 14)
		STEP(G, b, c, d, a, GET(8), 0x455a14ed, 20)
		STEP(G, a, b, c, d, GET(13), 0xa9e3e905, 5)
		STEP(G, d, a, b, c, GET(2), 0xfcefa3f8, 9)
		STEP(G, c, d, a, b, GET(7), 0x676f02d9, 14)
		STEP(G, b, c, d, a, GET(12), 0x8d2a4c8a, 20)

		STEP(H, a, b, c, d, GET(5), 0xfffa3942, 4)
		STEP(H, d, a, b, c, GET(8), 0x8771f681, 11)
		STEP(H, c, d, a, b, GET(11), 0x6d9d6122, 16)
		STEP(H, b, c, d, a, GET(14), 0xfde5380c, 23)
		STEP(H, a, b, c, d, GET(1), 0xa4beea44, 4)
		STEP(H, d, a, b, c, GET(4), 0x4bdecfa9, 11)
		STEP(H, c, d, a, b, GET(7), 0xf6bb4b60, 16)
		STEP(H, b, c, d, a, GET(10), 0xbebfbc70, 23)
		STEP(H, a, b, c, d, GET(13), 0x289b7ec6, 4)
		STEP(H, d, a, b, c, GET(0), 0xeaa127fa, 11)
		STEP(H, c, d, a, b, GET(3), 0xd4ef3085, 16)
		STEP(H, b, c, d, a, GET(6), 0x04881d05, 23)
		STEP(H, a, b, c, d, GET(9), 0xd9d4d039, 4)
		STEP(H, d, a, b, c, GET(12), 0xe6db99e5, 11)
		STEP(H, c, d, a, b, GET(15), 0x1fa27cf8, 16)
		STEP(H, b, c, d, a, GET(2), 0xc4ac5665, 23)

		STEP(I, a, b, c, d, GET(0), 0xf4292244, 6)
		STEP(I, d, a, b, c, GET(7), 0x432aff97, 10)
		STEP(I, c, d, a, b, GET(14), 0xab9423a7, 15)
		STEP(I, b, c, d, a, GET(5), 0xfc93a039, 21)
		STEP(I, a, b, c, d, GET(12), 0x655b59c3, 6)
		STEP(I, d, a, b, c, GET(3), 0x8f0ccc92, 10)
		STEP(I, c, d, a, b, GET(10), 0xffeff47d, 15)
		STEP(I, b, c, d, a, GET(1), 0x85845dd1, 21)
		STEP(I, a, b, c, d, GET(8), 0x6fa87e4f, 6)
		STEP(I, d, a, b, c, GET(15), 0xfe2ce6e0, 10)
		STEP(I, c, d, a, b, GET(6), 0xa3014314, 15)
		STEP(I, b, c, d, a, GET(13), 0x4e0811a1, 21)
		STEP(I, a, b, c, d, GET(4), 0xf7537e82, 6)
		STEP(I, d, a, b, c, GET(11), 0xbd3af235, 10)
		STEP(I, c, d, a, b, GET(2), 0x2ad7d2bb, 15)
		STEP(I, b, c, d, a, GET(9), 0xeb86d391, 21)

		a += savedA;
		b += savedB;
		c += savedC;
		d += savedD;

		ptr += 64;
	} while (size -= 64);

	ctx->a = a;
	ctx->b = b;
	ctx->c = c;
	ctx->d = d;

	return ptr;
}

inline void Md5Update(Md5Ctx* ctx, const void* data, size_t size)
{
	const auto savedLo = ctx->lo;
	if ((ctx->lo = (savedLo + size) & 0x1fffffff) < savedLo)
	{
		ctx->hi++;
	}
	ctx->hi += size >> 29;

	const auto used = savedLo & 0x3f;

	if (used)
	{
		const auto free = 64 - used;

		if (size < free)
		{
			memcpy(&ctx->buffer[used], data, size);
			return;
		}

		memcpy(&ctx->buffer[used], data, free);
		data = (unsigned char*)data + free;
		size -= free;
		Md5Body(ctx, ctx->buffer, 64);
	}

	if (size >= 64)
	{
		data = Md5Body(ctx, data, size & ~static_cast<size_t>(0x3f));
		size &= 0x3f;
	}

	memcpy(ctx->buffer, data, size);
}

inline void Md5Final(unsigned char* result, Md5Ctx* ctx)
{
	auto used = ctx->lo & 0x3f;

	ctx->buffer[used++] = 0x80;

	auto free = 64 - used;

	if (free < 8)
	{
		memset(&ctx->buffer[used], 0, free);
		Md5Body(ctx, ctx->buffer, 64);
		used = 0;
		free = 64;
	}

	memset(&ctx->buffer[used], 0, free - 8);

	ctx->lo <<= 3;
	ctx->buffer[56] = ctx->lo;
	ctx->buffer[57] = ctx->lo >> 8;
	ctx->buffer[58] = ctx->lo >> 16;
	ctx->buffer[59] = ctx->lo >> 24;
	ctx->buffer[60] = ctx->hi;
	ctx->buffer[61] = ctx->hi >> 8;
	ctx->buffer[62] = ctx->hi >> 16;
	ctx->buffer[63] = ctx->hi >> 24;

	Md5Body(ctx, ctx->buffer, 64);

	result[0] = ctx->a;
	result[1] = ctx->a >> 8;
	result[2] = ctx->a >> 16;
	result[3] = ctx->a >> 24;
	result[4] = ctx->b;
	result[5] = ctx->b >> 8;
	result[6] = ctx->b >> 16;
	result[7] = ctx->b >> 24;
	result[8] = ctx->c;
	result[9] = ctx->c >> 8;
	result[10] = ctx->c >> 16;
	result[11] = ctx->c >> 24;
	result[12] = ctx->d;
	result[13] = ctx->d >> 8;
	result[14] = ctx->d >> 16;
	result[15] = ctx->d >> 24;

	memset(ctx, 0, sizeof(*ctx));
}

inline void Md5MakeDigestEx(char* md5Str, const unsigned char* digest, const int len)
{
	static const char Hexits[17] = "0123456789abcdef";

	for (auto i = 0; i < len; i++)
	{
		md5Str[i * 2] = Hexits[digest[i] >> 4];
		md5Str[i * 2 + 1] = Hexits[digest[i] & 0x0F];
	}
	md5Str[len * 2] = 0;
}

std::string Md5File(const NativeStringType& path)
{
	char hashStr[33] = { 0 };
	unsigned char buf[4096] = { 0 };
	unsigned char digest[16] = { 0 };
	Md5Ctx context;
	size_t n;

	const auto stream = FileOpen(path.c_str(), "rb");
	if (!stream) return hashStr;

	Md5Init(&context);
	while ((n = fread(buf, sizeof(char), 4096, stream)) > 0) Md5Update(&context, buf, n);
	if (!feof(stream))
	{
		fclose(stream);
		Md5Final(digest, &context);
		return hashStr;
	}
	fclose(stream);
	Md5Final(digest, &context);
	Md5MakeDigestEx(hashStr, digest, 16);

	return hashStr;
}

#pragma endregion

#pragma region TupleSerialization

// see https://github.com/Sydius/serialize-tuple/blob/master/serialize_tuple.h

namespace boost::serialization
{
	template <uint32_t N>
	struct Serialize
	{
		template <class Archive, typename... Args>
		static void serialize(Archive& ar, std::tuple<Args...>& t, const unsigned int version)
		{
			ar& std::get<N - 1>(t);
			Serialize<N - 1>::serialize(ar, t, version);
		}
	};

	template <>
	struct Serialize<0>
	{
		template <class Archive, typename... Args>
		static void serialize(Archive& ar, std::tuple<Args...>& t, const unsigned int version)
		{
			(void)ar;
			(void)t;
			(void)version;
		}
	};

	template <class Archive, typename... Args>
	void serialize(Archive& ar, std::tuple<Args...>& t, const unsigned int version)
	{
		Serialize<sizeof...(Args)>::serialize(ar, t, version);
	}
}

#pragma endregion 

std::string FileLastModified(const NativeStringType& path)
{
	try
	{
		std::ostringstream oss{};
		const auto lwt = last_write_time(boost::filesystem::path(path));
		if (lwt < 0)
		{
			LogErr(path.c_str(), "get last_write_time() failed.");
			return "";
		}
		oss << std::put_time(static_cast<const tm*>(std::localtime(static_cast<time_t const*>(&lwt))), "%F %T");
		return oss.str();
	}
	catch (const std::exception& e)
	{
		LogErr(path.c_str(), e.what());
		return "";
	}
}

void FileMd5DatabaseBuilder(const std::string& deviceName, const std::string& path, Database& fmd, const bool logOutput = true)
{
	std::error_code errorCode;
	const std::error_code nonErrorCode;
	std::deque<std::filesystem::path> queue{};
	const std::filesystem::directory_iterator end;
	queue.emplace_back(std::filesystem::path(path));
	while (!queue.empty())
	{
		try
		{
			for (std::filesystem::directory_iterator file(queue.front(), errorCode); file != end; ++file)
			{
				if (errorCode != nonErrorCode)
				{
					LogErr(file->path().native().c_str(), errorCode.message().c_str());
					errorCode.clear();
					continue;
				}
				if (file->is_regular_file())
				{
					try
					{
						const auto nativePath = PathPerfix file->path().native();
						const auto fullPath = deviceName + ":" + file->path().u8string();
						const auto md5 = Md5File(nativePath);
						uintmax_t size;
						try
						{
							size = file->file_size();
						}
						catch (const std::exception& e)
						{
							LogErr(file->path().native().c_str(), e.what());
							size = 0;
						}
						const auto modificationTime = FileLastModified(nativePath);
						fmd.insert(KV(fullPath, std::make_tuple(md5, size, FileLastModified(file->path().native()))));
						if (logOutput) Log(file->path().native().c_str(), md5.c_str(), static_cast<uint64_t>(size), modificationTime.c_str());
					}
					catch (const std::exception& e)
					{
						LogErr(file->path().native().c_str(), e.what());
					}
				}
				else if (file->is_directory())
				{
					queue.emplace_back(file->path());
				}
			}
		}
		catch (const std::exception& e)
		{
			LogErr(queue.front().native().c_str(), e.what());
		}
		queue.pop_front();
	}
}

void Serialization(Database& fmd, const std::string& databasePath)
{
	try
	{
		std::ofstream dbFile(databasePath, std::ios::binary);
		boost::archive::binary_oarchive db(dbFile);
		db << fmd;
		dbFile.close();
	}
	catch (const std::exception& e)
	{
		fprintf(stderr, Line":%s:%s\n", databasePath.c_str(), e.what());
		exit(EXIT_FAILURE);
	}
}

void Deserialization(Database& fmd, const std::string& databasePath)
{
	try
	{
		std::ifstream dbFile(databasePath, std::ios::binary);
		boost::archive::binary_iarchive db(dbFile);
		db >> fmd;
		dbFile.close();
	}
	catch (const std::exception& e)
	{
		fprintf(stderr, Line":%s:%s\n", databasePath.c_str(), e.what());
		exit(EXIT_FAILURE);
	}
}

void FileMd5DatabaseQuery(Database& fmd, const std::string& queryMethod, const std::string& queryData, const std::string& keyword)
{
#define Search(init,value,judge)\
{\
	int count = 0;\
	init;\
	OmpParallel\
	{\
		size_t cnt = 0;\
		int ithread = omp_get_thread_num();\
		int nthreads = omp_get_num_threads();\
		for(auto pair = fmd.begin(); pair != fmd.end(); ++pair, cnt++)\
		{\
			if(cnt % nthreads != ithread) continue;\
			const auto data = value;\
			if (judge)\
			{\
				printf("<%s,<%s,%" PRIu64 ",%s>>\n", pair->first.c_str(), std::get<0>(pair->second).c_str(), std::get<1>(pair->second), std::get<2>(pair->second).c_str());\
			}\
		}\
	}\
}

#define ContainSearch(value) Search(,value,data.find(keyword) != std::string::npos)
#define StartWithSearch(value) Search(,value,StartWithFunc)
#define RegexSearch(value) Search(std::regex re(keyword),value,std::regex_match(data, re))

	if (queryMethod == "contain" && queryData == "path") ContainSearch(pair->first)
	else if (queryMethod == "contain" && queryData == "md5") ContainSearch(std::get<0>(pair->second))
	else if (queryMethod == "contain" && queryData == "fileModificationTime") ContainSearch(std::get<2>(pair->second))
	else if (queryMethod == "startWith" && queryData == "path") StartWithSearch(pair->first)
	else if (queryMethod == "startWith" && queryData == "md5") StartWithSearch(std::get<0>(pair->second))
	else if (queryMethod == "startWith" && queryData == "fileModificationTime") StartWithSearch(std::get<2>(pair->second))
	else if (queryMethod == "regex" && queryData == "path") RegexSearch(pair->first)
	else if (queryMethod == "regex" && queryData == "md5") RegexSearch(std::get<0>(pair->second))
	else if (queryMethod == "regex" && queryData == "fileModificationTime") RegexSearch(std::get<2>(pair->second))
	else if (queryMethod == "eq" && queryData == "md5&size")
	{
		std::map<std::string, uint64_t> md5 {};
		for (auto& pair : fmd)
		{
			const auto m = std::get<0>(pair.second);
			const auto s = std::get<1>(pair.second);
			if (!m.empty() && s != 0)
			{
				if (md5.find(m) == md5.end())
				{
					md5.insert(std::make_pair(m, s));
				}
				else
				{
					if (md5[m] != s)
					{
						printf("<%s,<%s,%" PRIu64 ",%s>>\n", pair.first.c_str(), std::get<0>(pair.second).c_str(), std::get<1>(pair.second), std::get<2>(pair.second).c_str());
					}
				}
			}
		}
	}
}

int main(int argc, char* argv[])
{
	try
	{
		if (argc >= 5)
		{
			const std::string databaseFilePath = argv[1];
			const std::string command = argv[2];
			if (command == "build" && argc <= 6)
			{
				const std::string deviceName = argv[3];
				const std::string rootPath = argv[4];
				const std::string logOutput = argc == 6 ? argv[5] : "true";
				if (std::filesystem::exists(databaseFilePath)) Deserialization(FileMd5Database, databaseFilePath);
#if (defined _WIN32 || defined _WIN64)
				setlocale(LC_ALL, "");
#endif
				FileMd5DatabaseBuilder(deviceName, rootPath, FileMd5Database, logOutput == "true");
				Serialization(FileMd5Database, databaseFilePath);
			}
			else if (command == "query" && argc >= 6 && argc <= 7)
			{
				const std::string queryMethod = argv[3];
				const std::string queryData = argv[4];
				const std::string keyword = argv[5];
				const auto limit = argc == 7 ? std::strtoull(argv[6], &argv[6], 10) : 100;
				Deserialization(FileMd5Database, databaseFilePath);
				printf("%" PRIu64 "\n", static_cast<uint64_t>(FileMd5Database.size()));

#if (defined _WIN32 || defined _WIN64)
				system("chcp 65001");
#endif

				FileMd5DatabaseQuery(FileMd5Database, queryMethod, queryData, keyword);
			}
			else if (command == "concat")
			{
				for (auto i = 3; i < argc; ++i)
				{
					Database temp{};
					Deserialization(temp, argv[i]);
					for (const auto& value : temp) FileMd5Database.insert(value);
				}
				Serialization(FileMd5Database, databaseFilePath);
			}
			else
			{
				throw ParameterError();
			}
		}
		else
		{
			throw ParameterError();
		}
	}
	catch (const std::exception& e)
	{
		fprintf(stderr, Line":%s\n", e.what());
		exit(EXIT_FAILURE);
	}
}
// g++ FileMd5Database.cpp -o FileMd5Database.out -std=c++17 -lboost_serialization -lboost_locale -lboost_filesystem -fopenmp -lboost_system -O2
// g++ FileMd5Database.cpp -o FileMd5Database.out -std=c++17 -l:libboost_serialization.a -l:libboost_locale.a -l:libboost_filesystem.a -fopenmp -l:libboost_system.a -O2
