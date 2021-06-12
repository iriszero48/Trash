#pragma once

#include <string>
#include <filesystem>
#include <charconv>

namespace Cryptography
{
	template<typename ValueType>
	class Hash
	{
	public:
		using HashValueType = ValueType;

		HashValueType Data{};

		Hash(const HashValueType& val);
		operator std::string() const;

		bool operator==(const std::string& hashStr) const;
		
		bool operator!=(const std::string& hashStr)
		{
			return !(*this == hashStr);
		}

		bool operator==(const Hash<HashValueType>& hash) const;
		
		bool operator!=(const Hash<HashValueType>& hash)
		{
			return !(*this == hash);
		}
	};
	
	template<typename T, typename HashValueType>
	class IHashAlgorithm
	{
	public:
		using HashType = Hash<HashValueType>;
		
		void Append(std::uint8_t* buf, std::uint64_t len)
		{
			static_cast<T*>(this)->Append(buf, len);
		}
		void Append(std::istream& stream)
		{
			static_cast<T*>(this)->Append(stream);
		}
		HashType Digest()
		{
			return static_cast<T*>(this)->Digest;
		}
	};

	class Md5: IHashAlgorithm<Md5, std::uint8_t[16]>
	{
	public:
		using HashValueType = std::uint8_t[16];
		
		Md5();
		void Append(std::uint8_t* buf, std::uint64_t len);
		void Append(std::istream& stream);
		HashType Digest();

	private:
		union DigestData
		{
			struct Integer { std::uint32_t A, B, C, D; } DWord;
			std::uint8_t Word[16];
		};
		
		DigestData data;
		std::uint8_t buffer[64]{0};
		std::uint8_t bufferLen = 0;
		std::uint64_t length = 0;
		bool finished = false;
		
		void Append64(std::uint8_t* buf, std::uint64_t n);
	};

	class Sha1: IHashAlgorithm<Sha1, std::uint8_t[20]>
	{
	public:
		using HashValueType = std::uint8_t[20];

		Sha1();
		void Append(std::uint8_t* buf, std::uint64_t len);
		void Append(std::istream& stream);
		HashType Digest();

	private:
		union DigestData
		{
			struct Integer { std::uint32_t A, B, C, D, E; } DWord;
			std::uint8_t Word[20];
		};

		DigestData data;
		std::uint8_t buffer[64]{ 0 };
		std::uint8_t bufferLen = 0;
		std::uint64_t length = 0;
		bool finished = false;

		void Append64(std::uint8_t* buf, std::uint64_t n);
	};

	class Sha256 : IHashAlgorithm<Sha256, std::uint8_t[32]>
	{
	public:
		using HashValueType = std::uint8_t[32];

		Sha256();
		void Append(std::uint8_t* buf, std::uint64_t len);
		void Append(std::istream& stream);
		HashType Digest();

	private:
		union DigestData
		{
			struct Integer { std::uint32_t A, B, C, D, E, F, G, H; } DWord;
			std::uint8_t Word[32];
		};

		DigestData data;
		std::uint8_t buffer[64]{ 0 };
		std::uint8_t bufferLen = 0;
		std::uint64_t length = 0;
		bool finished = false;

		void Append64(std::uint8_t* buf, std::uint64_t n);
	};

	class Crc32: IHashAlgorithm<Crc32, std::uint32_t>
	{
	public:
		using HashValueType = std::uint32_t;

		Crc32();
		void Append(std::uint8_t* buf, std::uint64_t len);
		void Append(std::istream& stream);
		HashType Digest();

	private:
		std::uint32_t data;
		bool finished = false;
	};
}
