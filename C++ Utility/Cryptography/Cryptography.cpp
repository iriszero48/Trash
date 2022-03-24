#include "Cryptography.h"

#include <stdexcept>
#include <cstring>
#include <numeric>
#include <algorithm>
#include <charconv>

namespace Detail
{
	namespace Bit
	{
		using __Detail::Bit::Endian;
		using __Detail::Bit::BSwap;
		
		template<typename T>
		constexpr T RotateLeft(const T x, const int n)
		{
			return (x << n) | (x >> (sizeof(T) * 8 - n));
		}
		
		template<typename T>
		constexpr T RotateRight(const T x, const int n)
		{
			return (x >> n) | (x << (sizeof(T) * 8 - n));
		}
	}

	namespace String
	{
		template<typename Func>
		struct NewString
		{
			Func Caller;

			explicit NewString(const Func caller) : Caller(caller) {}

			template<typename T, typename ...Args>
			std::string operator()(const T str, Args&&...args)
			{
				std::string buf(str);
				Caller(buf, std::forward<Args>(args)...);
				return buf;
			}
		};

		template<typename T>
		void ToLower(T& string)
		{
			std::transform(string.begin(), string.end(), string.begin(), static_cast<int(*)(int)>(std::tolower));
		}
	}

	namespace Hash
	{
		template<typename T>
		inline std::string Uint8ArrayToStringPaddingZero(const T& data)
		{
			std::string hex{};
			hex.reserve(sizeof(data) * 2);
			for (unsigned char value : data)
			{
				char res[4]{ '0', 0, 0, 0 };
				if (const auto [p, e] = std::to_chars(res + 1, res + 3, value, 16);
					e != std::errc{}) throw std::runtime_error("convert error");
				hex.append(res[2] == 0 ? std::string_view(res, 2) : std::string_view(res + 1, 2));
			}
			return hex;
		}

		template<typename T>
		inline std::string ToStringPaddingZero(const T value)
		{
			constexpr auto size = sizeof(T);
			constexpr auto bufSize = size * 2 + 1;

			char res[bufSize]{ 0 };
			if (const auto [p, e] = std::to_chars(res, res + bufSize, value, 16);
				e != std::errc{}) throw std::runtime_error("convert error");
			const std::string hex(res);
			return std::string(bufSize - 1 - hex.length(), '0') + hex;
		}

		template<typename T>
		inline bool ArrayCmp(const T& l, const T& r)
		{
			return std::equal(std::begin(l), std::end(l), std::begin(r));
		}

		template<typename T>
		inline bool HashStrCmp(const T& hash, const std::string& str)
		{
			return hash.operator std::string() == String::NewString(Detail::String::ToLower<std::string>)(str);
		}

	}
	
	namespace Md5
	{
		enum class Round { F, G, H, I };

		constexpr std::uint32_t F(const std::uint32_t x, const std::uint32_t y, const std::uint32_t z)
		{
			return z ^ x & (y ^ z);
		}

		constexpr std::uint32_t G(const std::uint32_t x, const std::uint32_t y, const std::uint32_t z)
		{
			return y ^ z & (x ^ y);
		}

		constexpr std::uint32_t H(const std::uint32_t x, const std::uint32_t y, const std::uint32_t z)
		{
			return x ^ y ^ z;
		}

		constexpr std::uint32_t I(const std::uint32_t x, const std::uint32_t y, const std::uint32_t z)
		{
			return y ^ (x | ~z);
		}

		template<Round Func>
		constexpr void Step(std::uint32_t& a, const std::uint32_t b, const std::uint32_t c, const std::uint32_t d, const std::uint32_t x, const std::uint32_t t, const std::uint32_t s)
		{
			if constexpr (Func == Round::F) a += F(b, c, d) + x + t;
			if constexpr (Func == Round::G) a += G(b, c, d) + x + t;
			if constexpr (Func == Round::H) a += H(b, c, d) + x + t;
			if constexpr (Func == Round::I) a += I(b, c, d) + x + t;
			a = a << s | (a >> (32u - s));
			a += b;
		}

		constexpr std::uint32_t Get(const std::uint8_t* buf, const std::uint64_t index)
		{
			using namespace Detail::Bit;
			if constexpr (Endian::Native == Endian::Little) return       *(std::uint32_t*)&buf[index * 4];
			if constexpr (Endian::Native == Endian::Big)    return BSwap(*(std::uint32_t*)&buf[index * 4]);
		}
	}

	namespace Sha1
	{
		enum class Round { F, G, H, I };

		constexpr std::uint32_t F(const std::uint32_t x, const std::uint32_t y, const std::uint32_t z)
		{
			return z ^ (x & (y ^ z));
		}

		constexpr std::uint32_t G(const std::uint32_t x, const std::uint32_t y, const std::uint32_t z)
		{
			return x ^ y ^ z;
		}

		constexpr std::uint32_t H(const std::uint32_t x, const std::uint32_t y, const std::uint32_t z)
		{
			return (x & y) | (z & (x | y));
		}

		constexpr std::uint32_t I(const std::uint32_t x, const std::uint32_t y, const std::uint32_t z)
		{
			return x ^ y ^ z;
		}

		constexpr std::uint32_t WAt(const std::uint32_t* w, const std::uint8_t i)
		{
			return w[i & 15u];
		}
		
		constexpr std::uint32_t W(std::uint32_t* w, const std::uint8_t i)
		{
			w[i & 15] = Bit::RotateLeft(WAt(w, i - 3) ^ WAt(w, i - 8) ^ WAt(w, i - 14) ^ WAt(w, i - 16), 1);
			return w[i & 15];
		}
		
		template<Round Func>
		constexpr void Step(std::uint32_t& a, std::uint32_t& b, const std::uint32_t c, const std::uint32_t d, std::uint32_t& e, const std::uint32_t w)
		{
			if constexpr (Func == Round::F) e += F(b, c, d) + w + 0x5A827999u;
			if constexpr (Func == Round::G) e += G(b, c, d) + w + 0x6ED9EBA1u;
			if constexpr (Func == Round::H) e += H(b, c, d) + w + 0x8F1BBCDCu;
			if constexpr (Func == Round::I) e += I(b, c, d) + w + 0xCA62C1D6u;
			e += Bit::RotateLeft(a, 5);
			b = Bit::RotateLeft(b, 30);
		}
	}

	namespace Sha256
	{
		static std::uint32_t K[] =
		{
			0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
			0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
			0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
			0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
			0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
			0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
			0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
			0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
		};
		
		constexpr std::uint32_t Ch(const std::uint32_t x, const std::uint32_t y, const std::uint32_t z)
		{
			return (x & y) ^ (~x & z);
		}

		constexpr std::uint32_t Maj(const std::uint32_t x, const std::uint32_t y, const std::uint32_t z)
		{
			return (x & y) ^ (x & z) ^ (y & z);
		}

		constexpr std::uint32_t S0(const std::uint32_t x)
		{
			return Bit::RotateRight(x, 2) ^ Bit::RotateRight(x, 13) ^ Bit::RotateRight(x, 22);
		}
		
		constexpr std::uint32_t S1(const std::uint32_t x)
		{
			return Bit::RotateRight(x, 6) ^ Bit::RotateRight(x, 11) ^ Bit::RotateRight(x, 25);
		}
		
		constexpr std::uint32_t R0(const std::uint32_t x)
		{
			return Bit::RotateRight(x, 7) ^ Bit::RotateRight(x, 18) ^ (x >> 3);
		}
		
		constexpr std::uint32_t R1(const std::uint32_t x)
		{
			return Bit::RotateRight(x, 17) ^ Bit::RotateRight(x, 19) ^ (x >> 10);
		}
	}
	
	namespace Crc32
	{
		static const uint32_t Table[] =
		{
			0x00000000UL, 0x77073096UL, 0xee0e612cUL, 0x990951baUL, 0x076dc419UL,
			0x706af48fUL, 0xe963a535UL, 0x9e6495a3UL, 0x0edb8832UL, 0x79dcb8a4UL,
			0xe0d5e91eUL, 0x97d2d988UL, 0x09b64c2bUL, 0x7eb17cbdUL, 0xe7b82d07UL,
			0x90bf1d91UL, 0x1db71064UL, 0x6ab020f2UL, 0xf3b97148UL, 0x84be41deUL,
			0x1adad47dUL, 0x6ddde4ebUL, 0xf4d4b551UL, 0x83d385c7UL, 0x136c9856UL,
			0x646ba8c0UL, 0xfd62f97aUL, 0x8a65c9ecUL, 0x14015c4fUL, 0x63066cd9UL,
			0xfa0f3d63UL, 0x8d080df5UL, 0x3b6e20c8UL, 0x4c69105eUL, 0xd56041e4UL,
			0xa2677172UL, 0x3c03e4d1UL, 0x4b04d447UL, 0xd20d85fdUL, 0xa50ab56bUL,
			0x35b5a8faUL, 0x42b2986cUL, 0xdbbbc9d6UL, 0xacbcf940UL, 0x32d86ce3UL,
			0x45df5c75UL, 0xdcd60dcfUL, 0xabd13d59UL, 0x26d930acUL, 0x51de003aUL,
			0xc8d75180UL, 0xbfd06116UL, 0x21b4f4b5UL, 0x56b3c423UL, 0xcfba9599UL,
			0xb8bda50fUL, 0x2802b89eUL, 0x5f058808UL, 0xc60cd9b2UL, 0xb10be924UL,
			0x2f6f7c87UL, 0x58684c11UL, 0xc1611dabUL, 0xb6662d3dUL, 0x76dc4190UL,
			0x01db7106UL, 0x98d220bcUL, 0xefd5102aUL, 0x71b18589UL, 0x06b6b51fUL,
			0x9fbfe4a5UL, 0xe8b8d433UL, 0x7807c9a2UL, 0x0f00f934UL, 0x9609a88eUL,
			0xe10e9818UL, 0x7f6a0dbbUL, 0x086d3d2dUL, 0x91646c97UL, 0xe6635c01UL,
			0x6b6b51f4UL, 0x1c6c6162UL, 0x856530d8UL, 0xf262004eUL, 0x6c0695edUL,
			0x1b01a57bUL, 0x8208f4c1UL, 0xf50fc457UL, 0x65b0d9c6UL, 0x12b7e950UL,
			0x8bbeb8eaUL, 0xfcb9887cUL, 0x62dd1ddfUL, 0x15da2d49UL, 0x8cd37cf3UL,
			0xfbd44c65UL, 0x4db26158UL, 0x3ab551ceUL, 0xa3bc0074UL, 0xd4bb30e2UL,
			0x4adfa541UL, 0x3dd895d7UL, 0xa4d1c46dUL, 0xd3d6f4fbUL, 0x4369e96aUL,
			0x346ed9fcUL, 0xad678846UL, 0xda60b8d0UL, 0x44042d73UL, 0x33031de5UL,
			0xaa0a4c5fUL, 0xdd0d7cc9UL, 0x5005713cUL, 0x270241aaUL, 0xbe0b1010UL,
			0xc90c2086UL, 0x5768b525UL, 0x206f85b3UL, 0xb966d409UL, 0xce61e49fUL,
			0x5edef90eUL, 0x29d9c998UL, 0xb0d09822UL, 0xc7d7a8b4UL, 0x59b33d17UL,
			0x2eb40d81UL, 0xb7bd5c3bUL, 0xc0ba6cadUL, 0xedb88320UL, 0x9abfb3b6UL,
			0x03b6e20cUL, 0x74b1d29aUL, 0xead54739UL, 0x9dd277afUL, 0x04db2615UL,
			0x73dc1683UL, 0xe3630b12UL, 0x94643b84UL, 0x0d6d6a3eUL, 0x7a6a5aa8UL,
			0xe40ecf0bUL, 0x9309ff9dUL, 0x0a00ae27UL, 0x7d079eb1UL, 0xf00f9344UL,
			0x8708a3d2UL, 0x1e01f268UL, 0x6906c2feUL, 0xf762575dUL, 0x806567cbUL,
			0x196c3671UL, 0x6e6b06e7UL, 0xfed41b76UL, 0x89d32be0UL, 0x10da7a5aUL,
			0x67dd4accUL, 0xf9b9df6fUL, 0x8ebeeff9UL, 0x17b7be43UL, 0x60b08ed5UL,
			0xd6d6a3e8UL, 0xa1d1937eUL, 0x38d8c2c4UL, 0x4fdff252UL, 0xd1bb67f1UL,
			0xa6bc5767UL, 0x3fb506ddUL, 0x48b2364bUL, 0xd80d2bdaUL, 0xaf0a1b4cUL,
			0x36034af6UL, 0x41047a60UL, 0xdf60efc3UL, 0xa867df55UL, 0x316e8eefUL,
			0x4669be79UL, 0xcb61b38cUL, 0xbc66831aUL, 0x256fd2a0UL, 0x5268e236UL,
			0xcc0c7795UL, 0xbb0b4703UL, 0x220216b9UL, 0x5505262fUL, 0xc5ba3bbeUL,
			0xb2bd0b28UL, 0x2bb45a92UL, 0x5cb36a04UL, 0xc2d7ffa7UL, 0xb5d0cf31UL,
			0x2cd99e8bUL, 0x5bdeae1dUL, 0x9b64c2b0UL, 0xec63f226UL, 0x756aa39cUL,
			0x026d930aUL, 0x9c0906a9UL, 0xeb0e363fUL, 0x72076785UL, 0x05005713UL,
			0x95bf4a82UL, 0xe2b87a14UL, 0x7bb12baeUL, 0x0cb61b38UL, 0x92d28e9bUL,
			0xe5d5be0dUL, 0x7cdcefb7UL, 0x0bdbdf21UL, 0x86d3d2d4UL, 0xf1d4e242UL,
			0x68ddb3f8UL, 0x1fda836eUL, 0x81be16cdUL, 0xf6b9265bUL, 0x6fb077e1UL,
			0x18b74777UL, 0x88085ae6UL, 0xff0f6a70UL, 0x66063bcaUL, 0x11010b5cUL,
			0x8f659effUL, 0xf862ae69UL, 0x616bffd3UL, 0x166ccf45UL, 0xa00ae278UL,
			0xd70dd2eeUL, 0x4e048354UL, 0x3903b3c2UL, 0xa7672661UL, 0xd06016f7UL,
			0x4969474dUL, 0x3e6e77dbUL, 0xaed16a4aUL, 0xd9d65adcUL, 0x40df0b66UL,
			0x37d83bf0UL, 0xa9bcae53UL, 0xdebb9ec5UL, 0x47b2cf7fUL, 0x30b5ffe9UL,
			0xbdbdf21cUL, 0xcabac28aUL, 0x53b39330UL, 0x24b4a3a6UL, 0xbad03605UL,
			0xcdd70693UL, 0x54de5729UL, 0x23d967bfUL, 0xb3667a2eUL, 0xc4614ab8UL,
			0x5d681b02UL, 0x2a6f2b94UL, 0xb40bbe37UL, 0xc30c8ea1UL, 0x5a05df1bUL,
			0x2d02ef8dUL
		};
	}

	namespace Base85Base
	{
		template<uint64_t B, uint64_t E> struct Pow { static const uint64_t Value = B * Pow<B, E - 1>::Value; };
		template<uint64_t B> struct Pow<B, static_cast<uint64_t>(0)> { static const uint64_t Value = 1; };

		constexpr uint32_t Char4AsUInt(const uint8_t* buf)
		{
			return (buf[0] << 24) | (buf[1] << 16) | (buf[2] << 8) | buf[3];
		}

		constexpr uint32_t Char5ToUInt(const std::string_view& tab, const uint8_t* buf)
		{
			return tab.find((char)buf[0]) * Pow<85, 4>::Value
				 + tab.find((char)buf[1]) * Pow<85, 3>::Value
				 + tab.find((char)buf[2]) * Pow<85, 2>::Value
				 + tab.find((char)buf[3]) * Pow<85, 1>::Value
				 + tab.find((char)buf[4]) * Pow<85, 0>::Value;
		}

		std::vector<uint8_t> Encode(const char* tab, const std::vector<uint8_t>& data)
		{
			std::vector<uint8_t> res{};

			const std::size_t lastBlockN = data.size() % 4;
			const std::size_t blockN = data.size() / 4 + (lastBlockN != 0 ? 1 : 0);

			for (std::size_t i = 0; i < blockN; i++)
			{
				uint32_t intV = 0;

				if (i == blockN - 1 && lastBlockN != 0)
				{
					uint8_t buf[4]{ 0 };
					std::copy_n(data.begin() + 4 * i, lastBlockN, buf);
					intV = Char4AsUInt(buf);
				}
				else
				{
					intV = Char4AsUInt(data.data() + 4 * i);
				}

				res.push_back(tab[(intV / Pow<85, 4>::Value) % 85]);
				res.push_back(tab[(intV / Pow<85, 3>::Value) % 85]);
				res.push_back(tab[(intV / Pow<85, 2>::Value) % 85]);
				res.push_back(tab[(intV / Pow<85, 1>::Value) % 85]);
				res.push_back(tab[(intV / Pow<85, 0>::Value) % 85]);
			}

			if (lastBlockN != 0)
			{
				const auto end = res.size() - (4 - lastBlockN);
				res.erase(res.begin() + end, res.end());
			}
			
			return res;
		}

		std::vector<uint8_t> Decode(const std::string_view& tab, const std::vector<uint8_t>& data)
		{
			std::vector<uint8_t> res{};

			const std::size_t lastBlockN = data.size() % 5;
			const std::size_t blockN = data.size() / 5 + (lastBlockN != 0 ? 1 : 0);

			for (std::size_t i = 0; i < blockN; i++)
			{
				uint32_t intV = 0;

				if (i == blockN - 1 && lastBlockN != 0)
				{
					uint8_t buf[5]{ tab[84], tab[84], tab[84], tab[84], tab[84] };
					std::copy_n(data.begin() + 5 * i, lastBlockN, buf);
					intV = Char5ToUInt(tab, buf);
				}
				else
				{
					intV = Char5ToUInt(tab, data.data() + 5 * i);
				}

				res.push_back((char)((intV >> 24) & 0xff));
				res.push_back((char)((intV >> 16) & 0xff));
				res.push_back((char)((intV >> 8) & 0xff));
				res.push_back((char)((intV >> 0) & 0xff));
			}

			if (lastBlockN != 0)
			{
				const auto end = res.size() - (5 - lastBlockN);
				res.erase(res.begin() + end, res.end());
			}

			return res;
		}
	}

	namespace Base85
	{
		static const char Table[] = { R"(!"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstu)" };
	}

	namespace Path85
	{
		static const char Table[] = { "!#$%&'()+,-.0123456789;=@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz{}~" };
	}
}

namespace Cryptography
{
#pragma region Md5
	template<>
	inline Hash<Md5::HashValueType>::Hash(const Md5::HashValueType& val)
	{
		std::copy(std::begin(val), std::end(val), std::begin(Data));
	}

	template<>
	 Hash<Md5::HashValueType>::operator std::string() const
	{
		return Detail::Hash::Uint8ArrayToStringPaddingZero(Data);
	}

	 template<>
	 bool Hash<Md5::HashValueType>::operator==(const std::string& hashStr) const
	 {
		 return Detail::Hash::HashStrCmp(*this, hashStr);
	 }

	 template<>
	 bool Hash<Md5::HashValueType>::operator==(const Hash<Md5::HashValueType>& hash) const
	 {
		 return Detail::Hash::ArrayCmp(Data, hash.Data);
	 }
	
	Md5::Md5(): data({{0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476}}) {}

	void Md5::Append(std::uint8_t* buf, std::uint64_t len)
	{
		if (finished) throw std::runtime_error("append error: finished");
		
		if (bufferLen != 0)
		{
			const std::uint64_t emp = 64u - bufferLen;
			if (len < emp)
			{
				memcpy(buf + bufferLen, buf, len);
				return;
			}
			memcpy(buf + bufferLen, buf, emp);
			buf += emp;
			len -= emp;
			Append64(buffer, 1);
			length += 64;
			bufferLen = 0;
		}
		
		const auto n = len >> 6u; // = len / 64
		const auto nByte = n * 64u;
		Append64(buf, n);
		length += nByte;
		bufferLen = len & 0x3fu; // = len % 64
		if (bufferLen != 0) memcpy(buffer, buf + nByte, bufferLen);
	}
	
	void Md5::Append(std::istream& stream)
	{
		if (finished) throw std::runtime_error("append error: finished");
		if (!stream) throw std::runtime_error("append error: bad stream");
		
		while (!stream.eof())
		{
			char buf[4096]{0};
			stream.read(buf, 4096);
			const auto count = stream.gcount();
			Append(reinterpret_cast<uint8_t*>(buf), count);
		}
	}

	Md5::HashType Md5::Digest()
	{
		using namespace Detail::Bit;
		
		if (finished) return data.Word;
		length += bufferLen;
		length <<= 3u; // *= 8
		buffer[bufferLen++] = 0x80u;
		
		auto emp = 64u - bufferLen;
		if (emp < 8u)
		{
			memset(buffer + bufferLen, 0u, emp);
			Append64(buffer, 1);
			bufferLen = 0;
			emp = 64u;
		}
		memset(buffer + bufferLen, 0, emp - 8u);
		
		if constexpr (Endian::Native == Endian::Big) length = BSwap(length);
		memcpy(buffer + 56, &length, 8);		
		Append64(buffer, 1);
		
		if constexpr (Endian::Native == Endian::Big)
		{
			data.DWord.A = BSwap(data.DWord.A);
			data.DWord.B = BSwap(data.DWord.B);
			data.DWord.C = BSwap(data.DWord.C);
			data.DWord.D = BSwap(data.DWord.D);
		}
		finished = true;
		return data.Word;
	}

	void Md5::Append64(std::uint8_t* buf, std::uint64_t n)
	{
		using namespace Detail::Md5;
		auto [a, b, c, d] = data.DWord;
		
		while (n--)
		{
			const auto savedA = a;
			const auto savedB = b;
			const auto savedC = c;
			const auto savedD = d;

			Step<Round::F>(a, b, c, d, Get(buf,  0), 0xd76aa478u,  7);
			Step<Round::F>(d, a, b, c, Get(buf,  1), 0xe8c7b756u, 12);
			Step<Round::F>(c, d, a, b, Get(buf,  2), 0x242070dbu, 17);
			Step<Round::F>(b, c, d, a, Get(buf,  3), 0xc1bdceeeu, 22);
			Step<Round::F>(a, b, c, d, Get(buf,  4), 0xf57c0fafu,  7);
			Step<Round::F>(d, a, b, c, Get(buf,  5), 0x4787c62au, 12);
			Step<Round::F>(c, d, a, b, Get(buf,  6), 0xa8304613u, 17);
			Step<Round::F>(b, c, d, a, Get(buf,  7), 0xfd469501u, 22);
			Step<Round::F>(a, b, c, d, Get(buf,  8), 0x698098d8u,  7);
			Step<Round::F>(d, a, b, c, Get(buf,  9), 0x8b44f7afu, 12);
			Step<Round::F>(c, d, a, b, Get(buf, 10), 0xffff5bb1u, 17);
			Step<Round::F>(b, c, d, a, Get(buf, 11), 0x895cd7beu, 22);
			Step<Round::F>(a, b, c, d, Get(buf, 12), 0x6b901122u,  7);
			Step<Round::F>(d, a, b, c, Get(buf, 13), 0xfd987193u, 12);
			Step<Round::F>(c, d, a, b, Get(buf, 14), 0xa679438eu, 17);
			Step<Round::F>(b, c, d, a, Get(buf, 15), 0x49b40821u, 22);
															  
			Step<Round::G>(a, b, c, d, Get(buf,  1), 0xf61e2562u,  5);
			Step<Round::G>(d, a, b, c, Get(buf,  6), 0xc040b340u,  9);
			Step<Round::G>(c, d, a, b, Get(buf, 11), 0x265e5a51u, 14);
			Step<Round::G>(b, c, d, a, Get(buf,  0), 0xe9b6c7aau, 20);
			Step<Round::G>(a, b, c, d, Get(buf,  5), 0xd62f105du,  5);
			Step<Round::G>(d, a, b, c, Get(buf, 10), 0x02441453u,  9);
			Step<Round::G>(c, d, a, b, Get(buf, 15), 0xd8a1e681u, 14);
			Step<Round::G>(b, c, d, a, Get(buf,  4), 0xe7d3fbc8u, 20);
			Step<Round::G>(a, b, c, d, Get(buf,  9), 0x21e1cde6u,  5);
			Step<Round::G>(d, a, b, c, Get(buf, 14), 0xc33707d6u,  9);
			Step<Round::G>(c, d, a, b, Get(buf,  3), 0xf4d50d87u, 14);
			Step<Round::G>(b, c, d, a, Get(buf,  8), 0x455a14edu, 20);
			Step<Round::G>(a, b, c, d, Get(buf, 13), 0xa9e3e905u,  5);
			Step<Round::G>(d, a, b, c, Get(buf,  2), 0xfcefa3f8u,  9);
			Step<Round::G>(c, d, a, b, Get(buf,  7), 0x676f02d9u, 14);
			Step<Round::G>(b, c, d, a, Get(buf, 12), 0x8d2a4c8au, 20);
															  
			Step<Round::H>(a, b, c, d, Get(buf,  5), 0xfffa3942u,  4);
			Step<Round::H>(d, a, b, c, Get(buf,  8), 0x8771f681u, 11);
			Step<Round::H>(c, d, a, b, Get(buf, 11), 0x6d9d6122u, 16);
			Step<Round::H>(b, c, d, a, Get(buf, 14), 0xfde5380cu, 23);
			Step<Round::H>(a, b, c, d, Get(buf,  1), 0xa4beea44u,  4);
			Step<Round::H>(d, a, b, c, Get(buf,  4), 0x4bdecfa9u, 11);
			Step<Round::H>(c, d, a, b, Get(buf,  7), 0xf6bb4b60u, 16);
			Step<Round::H>(b, c, d, a, Get(buf, 10), 0xbebfbc70u, 23);
			Step<Round::H>(a, b, c, d, Get(buf, 13), 0x289b7ec6u,  4);
			Step<Round::H>(d, a, b, c, Get(buf,  0), 0xeaa127fau, 11);
			Step<Round::H>(c, d, a, b, Get(buf,  3), 0xd4ef3085u, 16);
			Step<Round::H>(b, c, d, a, Get(buf,  6), 0x04881d05u, 23);
			Step<Round::H>(a, b, c, d, Get(buf,  9), 0xd9d4d039u,  4);
			Step<Round::H>(d, a, b, c, Get(buf, 12), 0xe6db99e5u, 11);
			Step<Round::H>(c, d, a, b, Get(buf, 15), 0x1fa27cf8u, 16);
			Step<Round::H>(b, c, d, a, Get(buf,  2), 0xc4ac5665u, 23);
															  
			Step<Round::I>(a, b, c, d, Get(buf,  0), 0xf4292244u,  6);
			Step<Round::I>(d, a, b, c, Get(buf,  7), 0x432aff97u, 10);
			Step<Round::I>(c, d, a, b, Get(buf, 14), 0xab9423a7u, 15);
			Step<Round::I>(b, c, d, a, Get(buf,  5), 0xfc93a039u, 21);
			Step<Round::I>(a, b, c, d, Get(buf, 12), 0x655b59c3u,  6);
			Step<Round::I>(d, a, b, c, Get(buf,  3), 0x8f0ccc92u, 10);
			Step<Round::I>(c, d, a, b, Get(buf, 10), 0xffeff47du, 15);
			Step<Round::I>(b, c, d, a, Get(buf,  1), 0x85845dd1u, 21);
			Step<Round::I>(a, b, c, d, Get(buf,  8), 0x6fa87e4fu,  6);
			Step<Round::I>(d, a, b, c, Get(buf, 15), 0xfe2ce6e0u, 10);
			Step<Round::I>(c, d, a, b, Get(buf,  6), 0xa3014314u, 15);
			Step<Round::I>(b, c, d, a, Get(buf, 13), 0x4e0811a1u, 21);
			Step<Round::I>(a, b, c, d, Get(buf,  4), 0xf7537e82u,  6);
			Step<Round::I>(d, a, b, c, Get(buf, 11), 0xbd3af235u, 10);
			Step<Round::I>(c, d, a, b, Get(buf,  2), 0x2ad7d2bbu, 15);
			Step<Round::I>(b, c, d, a, Get(buf,  9), 0xeb86d391u, 21);

			a += savedA;
			b += savedB;
			c += savedC;
			d += savedD;
			
			buf += 64;
		}
		data.DWord.A = a;
		data.DWord.B = b;
		data.DWord.C = c;
		data.DWord.D = d;
	}
#pragma endregion Md5

#pragma region Sha1
	template<>
	inline Hash<Sha1::HashValueType>::Hash(const Sha1::HashValueType& val)
	{
		std::copy(std::begin(val), std::end(val), std::begin(Data));
	}

	template<>
	Hash<Sha1::HashValueType>::operator std::string() const
	{
		return Detail::Hash::Uint8ArrayToStringPaddingZero(Data);
	}

	template<>
	bool Hash<Sha1::HashValueType>::operator==(const std::string& hashStr) const
	{
		return Detail::Hash::HashStrCmp(*this, hashStr);
	}

	template<>
	bool Hash<Sha1::HashValueType>::operator==(const Hash<Sha1::HashValueType>& hash) const
	{
		return Detail::Hash::ArrayCmp(Data, hash.Data);
	}
	
	Sha1::Sha1(): data({{0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476, 0xc3d2e1f0}}) {}

	void Sha1::Append(std::uint8_t* buf, std::uint64_t len)
	{
		if (finished) throw std::runtime_error("append error: finished");

		if (bufferLen != 0)
		{
			const std::uint64_t emp = 64u - bufferLen;
			if (len < emp)
			{
				memcpy(buf + bufferLen, buf, len);
				return;
			}
			memcpy(buf + bufferLen, buf, emp);
			buf += emp;
			len -= emp;
			Append64(buffer, 1);
			length += 64;
			bufferLen = 0;
		}

		const auto n = len >> 6u; // = len / 64
		const auto nByte = n * 64u;
		Append64(buf, n);
		length += nByte;
		bufferLen = len & 0x3fu; // = len % 64
		if (bufferLen != 0) memcpy(buffer, buf + nByte, bufferLen);
	}
	
	void Sha1::Append(std::istream& stream)
	{
		if (finished) throw std::runtime_error("append error: finished");
		if (!stream) throw std::runtime_error("append error: bad stream");

		while (!stream.eof())
		{
			char buf[4096]{ 0 };
			stream.read(buf, 4096);
			const auto count = stream.gcount();
			Append(reinterpret_cast<uint8_t*>(buf), count);
		}
	}

	Sha1::HashType Sha1::Digest()
	{
		using namespace Detail::Bit;
		
		if (finished) return data.Word;
		length += bufferLen;
		length <<= 3u; // *= 8
		buffer[bufferLen++] = 0x80u;

		auto emp = 64u - bufferLen;
		if (emp < 8u)
		{
			memset(buffer + bufferLen, 0u, emp);
			Append64(buffer, 1);
			bufferLen = 0;
			emp = 64u;
		}
		memset(buffer + bufferLen, 0, emp - 8u);

		if constexpr (Endian::Native == Endian::Little) length = BSwap(length);
		memcpy(buffer + 56, &length, 8);
		Append64(buffer, 1);

		if constexpr (Endian::Native == Endian::Little)
		{
			data.DWord.A = BSwap(data.DWord.A);
			data.DWord.B = BSwap(data.DWord.B);
			data.DWord.C = BSwap(data.DWord.C);
			data.DWord.D = BSwap(data.DWord.D);
			data.DWord.E = BSwap(data.DWord.E);
		}
		finished = true;
		return data.Word;
	}

	void Sha1::Append64(std::uint8_t* buf, std::uint64_t n)
	{
		using namespace Detail::Sha1;
		using namespace Detail::Bit;
		
		auto [a, b, c, d, e] = data.DWord;

		while (n--)
		{
			const auto savedA = a;
			const auto savedB = b;
			const auto savedC = c;
			const auto savedD = d;
			const auto savedE = e;

			std::uint32_t w[16];
			for (int i = 0; i < 16; ++i)
			{
				if constexpr (Endian::Native == Endian::Little) w[i] = BSwap(*(std::uint32_t*)&buf[i * 4]);
				if constexpr (Endian::Native == Endian::Big   ) w[i] =      (*(std::uint32_t*)&buf[i * 4]);
			}

			Step<Round::F>(a, b, c, d, e,   w [ 0]); 
			Step<Round::F>(e, a, b, c, d,   w [ 1]); 
			Step<Round::F>(d, e, a, b, c,   w [ 2]); 
			Step<Round::F>(c, d, e, a, b,   w [ 3]); 
			Step<Round::F>(b, c, d, e, a,   w [ 4]); 
			Step<Round::F>(a, b, c, d, e,   w [ 5]); 
			Step<Round::F>(e, a, b, c, d,   w [ 6]); 
			Step<Round::F>(d, e, a, b, c,   w [ 7]); 
			Step<Round::F>(c, d, e, a, b,   w [ 8]); 
			Step<Round::F>(b, c, d, e, a,   w [ 9]); 
			Step<Round::F>(a, b, c, d, e,   w [10]);
			Step<Round::F>(e, a, b, c, d,   w [11]);
			Step<Round::F>(d, e, a, b, c,   w [12]);
			Step<Round::F>(c, d, e, a, b,   w [13]);
			Step<Round::F>(b, c, d, e, a,   w [14]);
			Step<Round::F>(a, b, c, d, e,   w [15]);
			Step<Round::F>(e, a, b, c, d, W(w, 16));
			Step<Round::F>(d, e, a, b, c, W(w, 17));
			Step<Round::F>(c, d, e, a, b, W(w, 18));
			Step<Round::F>(b, c, d, e, a, W(w, 19));

			Step<Round::G>(a, b, c, d, e, W(w, 20));
			Step<Round::G>(e, a, b, c, d, W(w, 21));
			Step<Round::G>(d, e, a, b, c, W(w, 22));
			Step<Round::G>(c, d, e, a, b, W(w, 23));
			Step<Round::G>(b, c, d, e, a, W(w, 24));
			Step<Round::G>(a, b, c, d, e, W(w, 25));
			Step<Round::G>(e, a, b, c, d, W(w, 26));
			Step<Round::G>(d, e, a, b, c, W(w, 27));
			Step<Round::G>(c, d, e, a, b, W(w, 28));
			Step<Round::G>(b, c, d, e, a, W(w, 29));
			Step<Round::G>(a, b, c, d, e, W(w, 30));
			Step<Round::G>(e, a, b, c, d, W(w, 31));
			Step<Round::G>(d, e, a, b, c, W(w, 32));
			Step<Round::G>(c, d, e, a, b, W(w, 33));
			Step<Round::G>(b, c, d, e, a, W(w, 34));
			Step<Round::G>(a, b, c, d, e, W(w, 35));
			Step<Round::G>(e, a, b, c, d, W(w, 36));
			Step<Round::G>(d, e, a, b, c, W(w, 37));
			Step<Round::G>(c, d, e, a, b, W(w, 38));
			Step<Round::G>(b, c, d, e, a, W(w, 39));

			Step<Round::H>(a, b, c, d, e, W(w, 40));
			Step<Round::H>(e, a, b, c, d, W(w, 41));
			Step<Round::H>(d, e, a, b, c, W(w, 42));
			Step<Round::H>(c, d, e, a, b, W(w, 43));
			Step<Round::H>(b, c, d, e, a, W(w, 44));
			Step<Round::H>(a, b, c, d, e, W(w, 45));
			Step<Round::H>(e, a, b, c, d, W(w, 46));
			Step<Round::H>(d, e, a, b, c, W(w, 47));
			Step<Round::H>(c, d, e, a, b, W(w, 48));
			Step<Round::H>(b, c, d, e, a, W(w, 49));
			Step<Round::H>(a, b, c, d, e, W(w, 50));
			Step<Round::H>(e, a, b, c, d, W(w, 51));
			Step<Round::H>(d, e, a, b, c, W(w, 52));
			Step<Round::H>(c, d, e, a, b, W(w, 53));
			Step<Round::H>(b, c, d, e, a, W(w, 54));
			Step<Round::H>(a, b, c, d, e, W(w, 55));
			Step<Round::H>(e, a, b, c, d, W(w, 56));
			Step<Round::H>(d, e, a, b, c, W(w, 57));
			Step<Round::H>(c, d, e, a, b, W(w, 58));
			Step<Round::H>(b, c, d, e, a, W(w, 59));

			Step<Round::I>(a, b, c, d, e, W(w, 60));
			Step<Round::I>(e, a, b, c, d, W(w, 61));
			Step<Round::I>(d, e, a, b, c, W(w, 62));
			Step<Round::I>(c, d, e, a, b, W(w, 63));
			Step<Round::I>(b, c, d, e, a, W(w, 64));
			Step<Round::I>(a, b, c, d, e, W(w, 65));
			Step<Round::I>(e, a, b, c, d, W(w, 66));
			Step<Round::I>(d, e, a, b, c, W(w, 67));
			Step<Round::I>(c, d, e, a, b, W(w, 68));
			Step<Round::I>(b, c, d, e, a, W(w, 69));
			Step<Round::I>(a, b, c, d, e, W(w, 70));
			Step<Round::I>(e, a, b, c, d, W(w, 71));
			Step<Round::I>(d, e, a, b, c, W(w, 72));
			Step<Round::I>(c, d, e, a, b, W(w, 73));
			Step<Round::I>(b, c, d, e, a, W(w, 74));
			Step<Round::I>(a, b, c, d, e, W(w, 75));
			Step<Round::I>(e, a, b, c, d, W(w, 76));
			Step<Round::I>(d, e, a, b, c, W(w, 77));
			Step<Round::I>(c, d, e, a, b, W(w, 78));
			Step<Round::I>(b, c, d, e, a, W(w, 79));

			a += savedA;
			b += savedB;
			c += savedC;
			d += savedD;
			e += savedE;

			buf += 64;
		}
		
		data.DWord.A = a;
		data.DWord.B = b;
		data.DWord.C = c;
		data.DWord.D = d;
		data.DWord.E = e;
	}
#pragma endregion Sha1

#pragma region Sha256
	template<>
	inline Hash<Sha256::HashValueType>::Hash(const Sha256::HashValueType& val)
	{
		std::copy(std::begin(val), std::end(val), std::begin(Data));
	}

	template<>
	Hash<Sha256::HashValueType>::operator std::string() const
	{
		return Detail::Hash::Uint8ArrayToStringPaddingZero(Data);
	}

	template<>
	bool Hash<Sha256::HashValueType>::operator==(const std::string& hashStr) const
	{
		return Detail::Hash::HashStrCmp(*this, hashStr);
	}

	template<>
	bool Hash<Sha256::HashValueType>::operator==(const Hash<Sha256::HashValueType>& hash) const
	{
		return Detail::Hash::ArrayCmp(Data, hash.Data);
	}

	
	Sha256::Sha256(): data({{
		0x6a09e667u,
		0xbb67ae85u,
		0x3c6ef372u,
		0xa54ff53au,
		0x510e527fu,
		0x9b05688cu,
		0x1f83d9abu,
		0x5be0cd19u
	}}) {}
	
	void Sha256::Append(std::uint8_t* buf, std::uint64_t len)
	{
		if (finished) throw std::runtime_error("append error: finished");

		if (bufferLen != 0)
		{
			const std::uint64_t emp = 64u - bufferLen;
			if (len < emp)
			{
				memcpy(buf + bufferLen, buf, len);
				return;
			}
			memcpy(buf + bufferLen, buf, emp);
			buf += emp;
			len -= emp;
			Append64(buffer, 1);
			length += 64;
			bufferLen = 0;
		}

		const auto n = len >> 6u; // = len / 64
		const auto nByte = n * 64u;
		Append64(buf, n);
		length += nByte;
		bufferLen = len & 0x3fu; // = len % 64
		if (bufferLen != 0) memcpy(buffer, buf + nByte, bufferLen);
	}

	void Sha256::Append(std::istream& stream)
	{
		if (finished) throw std::runtime_error("append error: finished");
		if (!stream) throw std::runtime_error("append error: bad stream");

		while (!stream.eof())
		{
			char buf[4096]{ 0 };
			stream.read(buf, 4096);
			const auto count = stream.gcount();
			Append(reinterpret_cast<uint8_t*>(buf), count);
		}
	}
	
	Sha256::HashType Sha256::Digest()
	{
		using namespace Detail::Bit;
		
		if (finished) return data.Word;
		length += bufferLen;
		length <<= 3u; // *= 8
		buffer[bufferLen++] = 0x80u;

		auto emp = 64u - bufferLen;
		if (emp < 8u)
		{
			memset(buffer + bufferLen, 0u, emp);
			Append64(buffer, 1);
			bufferLen = 0;
			emp = 64u;
		}
		memset(buffer + bufferLen, 0, emp - 8u);

		if constexpr (Endian::Native == Endian::Little) length = BSwap(length);
		memcpy(buffer + 56, &length, 8);
		Append64(buffer, 1);

		if constexpr (Endian::Native == Endian::Little)
		{
			data.DWord.A = BSwap(data.DWord.A);
			data.DWord.B = BSwap(data.DWord.B);
			data.DWord.C = BSwap(data.DWord.C);
			data.DWord.D = BSwap(data.DWord.D);
			data.DWord.E = BSwap(data.DWord.E);
			data.DWord.F = BSwap(data.DWord.F);
			data.DWord.G = BSwap(data.DWord.G);
			data.DWord.H = BSwap(data.DWord.H);
		}
		finished = true;
		return data.Word;
	}

	void Sha256::Append64(std::uint8_t* buf, std::uint64_t n)
	{
		using namespace Detail::Sha256;
		using namespace Detail::Bit;
		auto [a, b, c, d, e, f, g, h] = data.DWord;

		while (n--)
		{
			const auto savedA = a;
			const auto savedB = b;
			const auto savedC = c;
			const auto savedD = d;
			const auto savedE = e;
			const auto savedF = f;
			const auto savedG = g;
			const auto savedH = h;

			std::uint32_t w[64];
			for (int i = 0; i < 16; ++i)
			{
				if constexpr (Endian::Native == Endian::Little) w[i] = BSwap(*(std::uint32_t*)&buf[i * 4]);
				if constexpr (Endian::Native == Endian::Big)    w[i] =      (*(std::uint32_t*)&buf[i * 4]);
			}

			for (auto i = 16; i < 64; ++i)
			{
				w[i ] = R1(w[(i - 2) ]) + w[(i - 7) ] + R0(w[(i - 15) ]) + w[(i - 16) ];
			}
			
			for (auto i = 0; i < 64; ++i)
			{
				const auto temp1 = h + S1(e) + Ch(e, f, g) + K[i] + w[i];
				const auto temp2 = S0(a) + Maj(a, b, c);
				h = g;
				g = f;
				f = e;
				e = d + temp1;
				d = c;
				c = b;
				b = a;
				a = temp1 + temp2;
			}
			
			a += savedA;
			b += savedB;
			c += savedC;
			d += savedD;
			e += savedE;
			f += savedF;
			g += savedG;
			h += savedH;

			buf += 64;
		}

		data.DWord.A = a;
		data.DWord.B = b;
		data.DWord.C = c;
		data.DWord.D = d;
		data.DWord.E = e;
		data.DWord.F = f;
		data.DWord.G = g;
		data.DWord.H = h;
	}
#pragma endregion Sha256
	
#pragma region Crc32
	template<>
	inline Hash<Crc32::HashValueType>::Hash(const Crc32::HashValueType& val)
	{
		Data = val;
	}

	template<>
	Hash<Crc32::HashValueType>::operator std::string() const
	{
		return Detail::Hash::ToStringPaddingZero(Data);
	}

	template<>
	bool Hash<Crc32::HashValueType>::operator==(const std::string& hashStr) const
	{
		return Detail::Hash::HashStrCmp(*this, hashStr);
	}

	template<>
	bool Hash<Crc32::HashValueType>::operator==(const Hash<Crc32::HashValueType>& hash) const
	{
		return Data == hash.Data;
	}

	Crc32::Crc32(): data(~std::uint32_t{ 0 }) {}
	
	void Crc32::Append(std::uint8_t* buf, const std::uint64_t len)
	{
		for (std::uint64_t i = 0u; i < len; ++i)
		{
			data = Detail::Crc32::Table[static_cast<uint8_t>(data ^ buf[i])] ^ data >> 8u;
		}
	}
	
	void Crc32::Append(std::istream& stream)
	{
		if (finished) throw std::runtime_error("append error: finished");
		if (!stream) throw std::runtime_error("append error: bad stream");

		while (!stream.eof())
		{
			char buf[4096]{ 0 };
			stream.read(buf, 4096);
			const auto count = stream.gcount();
			Append(reinterpret_cast<uint8_t*>(buf), count);
		}
	}
	Crc32::HashType Crc32::Digest()
	{
		if (finished) return data;
		data = ~data;
		finished = true;
		return data;
	}
#pragma endregion Crc32

#pragma region Base85
	std::vector<uint8_t> Base85::Encode(const std::vector<uint8_t>& data)
	{
		return Detail::Base85Base::Encode(Detail::Base85::Table, data);
	}

	std::vector<uint8_t> Base85::Decode(const std::vector<uint8_t>& data)
	{
		return Detail::Base85Base::Decode(std::string_view(Detail::Base85::Table, 85), data);
	}
#pragma endregion Base85

#pragma region Path85
	std::vector<uint8_t> Path85::Encode(const std::vector<uint8_t>& data)
	{
		return Detail::Base85Base::Encode(Detail::Path85::Table, data);
	}

	std::vector<uint8_t> Path85::Decode(const std::vector<uint8_t>& data)
	{
		return Detail::Base85Base::Decode(std::string_view(Detail::Path85::Table, 85), data);
	}
#pragma endregion Path85
}
