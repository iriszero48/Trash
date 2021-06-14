#include <iostream>
#include <fstream>
#include <memory>

#include "Cryptography.h"
//#define SHA1
int main(int argc, char* argv[])
{
	std::ifstream fs(argv[1], std::ios::in | std::ios::binary);
	const auto buf = std::make_unique<char[]>(4096);
	fs.rdbuf()->pubsetbuf(buf.get(), 4096);
	
#ifdef MD5
	Cryptography::Md5 md5{};
	md5.Append(fs);
	const auto hash = md5.Digest();
#endif

#ifdef SHA1
	Cryptography::Sha1 sha1{};
	sha1.Append(fs);
	const auto hash = sha1.Digest();
#endif

#ifdef SHA256
	Cryptography::Sha256 sha256{};
	sha256.Append(fs);
	const auto hash = sha256.Digest();
#endif
	
#ifdef CRC32
	Cryptography::Crc32 crc32{};
	crc32.Append(fs);
	const auto hash = crc32.Digest();
#endif
	
	const std::string res = hash;
	std::cout << res << std::endl;
}
