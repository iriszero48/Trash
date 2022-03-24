#pragma once

#include <string>
#include <iostream>
#include <algorithm>
#include <execution>
#include <climits>
#include <array>

namespace __Detail
{
	namespace Bit
	{
		enum class Endian
		{
#ifdef _MSC_VER
			Little = 0,
			Big = 1,
			Native = Little
#else
			Little = __ORDER_LITTLE_ENDIAN__,
			Big = __ORDER_BIG_ENDIAN__,
			Native = __BYTE_ORDER__
#endif
		};

		template<typename T, std::size_t... N>
		constexpr T BSwapImpl(T i, std::index_sequence<N...>)
		{
			return (((i >> N * CHAR_BIT & static_cast<std::uint8_t>(-1)) << (sizeof(T) - 1 - N) * CHAR_BIT) | ...);
		}

		template<typename T, typename U = std::make_unsigned_t<T>>
		constexpr U BSwap(T i)
		{
			return BSwapImpl<U>(i, std::make_index_sequence<sizeof(T)>{});
		}
	}

	namespace Enumerable
	{
		template<typename T>
		class Range
		{
			T begVal = 0;
			T endVal;

		public:
			class iterator
			{
				T val;

			public:
				using iterator_category = std::random_access_iterator_tag;
				using value_type = T;
				using difference_type = std::uint64_t;
				using pointer = const T*;
				using reference = const T;

				iterator() : val(0) {}
				iterator(T val) : val(val) {}

				iterator& operator++() { ++val; return *this; }
				iterator operator++(int) { iterator retVal = *this; ++(*this); return retVal; }

				bool operator==(iterator other) const { return val == other.val; }
				bool operator!=(iterator other) const { return !(*this == other); }
				bool operator<(iterator other) const { return val < other.val; }

				reference operator*() const { return val; }
				value_type operator+(iterator other) const { return val + other.val; }
				difference_type operator-(iterator other) const { return val - other.val; }

				reference operator[](const difference_type off) const { return val + off; }
			};

			Range(T count) :endVal(count) {}
			Range(T start, T count) : begVal(start), endVal(start + count) {}

			iterator begin() { return iterator(begVal); }
			iterator end() { return iterator(endVal); }
		};
	}
}

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
		
		bool operator!=(const std::string& hashStr) const
		{
			return !(*this == hashStr);
		}

		bool operator==(const Hash<HashValueType>& hash) const;
		
		bool operator!=(const Hash<HashValueType>& hash) const
		{
			return !(*this == hash);
		}
	};
	
	template<typename T, typename Type>
	class IHashAlgorithm
	{
	public:
		using HashValueType = Type;
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
		using HashValueType = HashValueType;
		
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
		using HashValueType = HashValueType;

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
		using HashValueType = HashValueType;

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
		using HashValueType = HashValueType;

		Crc32();
		void Append(std::uint8_t* buf, std::uint64_t len);
		void Append(std::istream& stream);
		HashType Digest();

	private:
		std::uint32_t data;
		bool finished = false;
	};

	template<typename T>
	class ISymmetricAlgorithm
	{
	public:
		void Encrypt(std::uint8_t* data, std::uint64_t dataLen)
		{
			static_cast<T*>(this)->Encrypt(data, dataLen);
		}
		
		void Decrypt(std::uint8_t* data, std::uint64_t dataLen)
		{
			static_cast<T*>(this)->Decrypt(data, dataLen);
		}
	};

	template<typename T, std::uint32_t Cycles = 32, bool LittleEndian = true, std::uint32_t Delta = 0x9E3779B9>
	class TeaBase : ISymmetricAlgorithm<TeaBase<T, Cycles, LittleEndian, Delta>>
	{
	public:
		struct Key
		{
			std::uint32_t A, B, C, D;
		};

		TeaBase() = delete;
		
		explicit TeaBase(const Key key) : key(key) {}

		explicit TeaBase(const uint8_t keyBytes[16])
		{
			using __Detail::Bit::Endian;
			using __Detail::Bit::BSwap;

			key = Key
			{
				*(uint32_t*)&keyBytes[0 * 4],
				*(uint32_t*)&keyBytes[1 * 4],
				*(uint32_t*)&keyBytes[2 * 4],
				*(uint32_t*)&keyBytes[3 * 4]
			};

			if constexpr (LittleEndian && Endian::Native == Endian::Big ||
				!LittleEndian && Endian::Native == Endian::Little)
			{
				key.A = BSwap(key.A);
				key.B = BSwap(key.B);
				key.C = BSwap(key.C);
				key.D = BSwap(key.D);
			}
		}

		void Encrypt(std::uint8_t* data, std::uint64_t dataLen) const
		{
			if (dataLen % 8 != 0) throw std::runtime_error("Encrypt error: block sizes must be 64 bits");

			for (std::uint64_t n = dataLen / 8, i = 0; i < n; ++i)
			{
				Call64<true>(data + i * 8);
			}
		}

		template<typename ExecutionPolicy>
		void Encrypt(ExecutionPolicy&& policy, std::uint8_t* data, std::uint64_t dataLen) const
		{
			if (dataLen % 8 != 0) throw std::runtime_error("Encrypt error: block sizes must be 64 bits");

			__Detail::Enumerable::Range rng(dataLen / 8);
			std::for_each(policy, rng.begin(), rng.end(), [&](const auto i)
			{
				Call64<true>(data + i * 8);
			});
		}

		void Decrypt(std::uint8_t* data, std::uint64_t dataLen) const
		{
			if (dataLen % 8 != 0) throw std::runtime_error("Decrypt error: block sizes must be 64 bits");

			for (std::uint64_t n = dataLen / 8, i = 0; i < n; ++i)
			{
				Call64<false>(data + i * 8);
			}
		}

		template<typename ExecutionPolicy>
		void Decrypt(ExecutionPolicy&& policy, std::uint8_t* data, std::uint64_t dataLen) const
		{
			if (dataLen % 8 != 0) throw std::runtime_error("Encrypt error: block sizes must be 64 bits");

			__Detail::Enumerable::Range rng(dataLen / 8);
			std::for_each(policy, rng.begin(), rng.end(), [&](const auto i)
			{
				Call64<false>(data + i * 8);
			});
		}

		const Key& GetKey() const
		{
			return key;
		}

	private:
		Key key;

	protected:
		void Encrypt64(std::uint32_t data[2]) const
		{
			static_cast<const T*>(this)->__Encrypt64(data);
		}

		void Decrypt64(uint32_t data[2]) const
		{
			static_cast<const T*>(this)->__Decrypt64(data);
		}

		template<bool Enc>
		void Call64(std::uint8_t data[8]) const
		{
			using __Detail::Bit::Endian;
			using __Detail::Bit::BSwap;

			std::uint32_t dt[2]
			{
				*(uint32_t*)&data[0 * 4],
				*(uint32_t*)&data[1 * 4]
			};

			if constexpr ((LittleEndian && Endian::Native == Endian::Big) ||
				(!LittleEndian && Endian::Native == Endian::Little))
			{
				dt[0] = BSwap(dt[0]);
				dt[1] = BSwap(dt[1]);
			}

			if constexpr (Enc) Encrypt64(dt);
			else Decrypt64(dt);

			if constexpr (LittleEndian && Endian::Native == Endian::Big ||
				!LittleEndian && Endian::Native == Endian::Little)
			{
				dt[0] = BSwap(dt[0]);
				dt[1] = BSwap(dt[1]);
			}

			*(uint32_t*)&data[0 * 4] = dt[0];
			*(uint32_t*)&data[1 * 4] = dt[1];
		}
	};
	
	template<std::uint32_t Cycles = 32, bool LittleEndian = true, std::uint32_t Delta = 0x9E3779B9>
	class Tea: public TeaBase<Tea<Cycles, LittleEndian, Delta>, Cycles, LittleEndian, Delta>
	{
	public:
		using Base = TeaBase<Tea<Cycles, LittleEndian, Delta>, Cycles, LittleEndian, Delta>;
		using typename Base::Key;
		using Base::Encrypt;
		using Base::Decrypt;

		explicit Tea(const Key key) : Base(key) {}
		explicit Tea(const uint8_t keyBytes[16]) : Base(keyBytes) {}
	
	private:
		using Base::GetKey;
	
	public:
		void __Encrypt64(std::uint32_t data[2]) const
		{
			std::uint32_t v0 = data[0];
			std::uint32_t v1 = data[1];
			std::uint32_t sum = 0;
			const auto [k0, k1, k2, k3] = GetKey();
			
			for (std::uint32_t i = 0; i < Cycles; i++)
			{
				sum += Delta;
				v0 += ((v1 << 4) + k0) ^ (v1 + sum) ^ ((v1 >> 5) + k1);
				v1 += ((v0 << 4) + k2) ^ (v0 + sum) ^ ((v0 >> 5) + k3);
			}
			
			data[0] = v0;
			data[1] = v1;
		}

		void __Decrypt64(uint32_t data[2]) const
		{
			std::uint32_t v0 = data[0];
			std::uint32_t v1 = data[1];
			std::uint32_t sum = (Delta * Cycles) & static_cast<std::uint32_t>(-1);
			const auto [k0, k1, k2, k3] = GetKey();
			
			for (std::uint32_t i = 0; i < Cycles; i++)
			{
				v1 -= ((v0 << 4) + k2) ^ (v0 + sum) ^ ((v0 >> 5) + k3);
				v0 -= ((v1 << 4) + k0) ^ (v1 + sum) ^ ((v1 >> 5) + k1);
				sum -= Delta;
			}
			data[0] = v0;
			data[1] = v1;
		}
	};

	template<std::uint32_t Cycles = 32, bool LittleEndian = true, std::uint32_t Delta = 0x9E3779B9>
	class Xtea : public TeaBase<Xtea<Cycles, LittleEndian, Delta>, Cycles, LittleEndian, Delta>
	{
	public:
		using Base = TeaBase<Xtea<Cycles, LittleEndian, Delta>, Cycles, LittleEndian, Delta>;
		using typename Base::Key;
		using Base::Encrypt;
		using Base::Decrypt;

		explicit Xtea(const Key key) : Base(key) {}
		explicit Xtea(const uint8_t keyBytes[16]) : Base(keyBytes) {}

	private:
		using Base::GetKey;

	public:
		void __Encrypt64(std::uint32_t data[2]) const
		{
			std::uint32_t v0 = data[0];
			std::uint32_t v1 = data[1];
			std::uint32_t sum = 0;
			const auto [k0, k1, k2, k3] = GetKey();
			const std::uint32_t key[]{ k0,k1,k2,k3 };
			
			for (auto i = 0; i < Cycles; i++)
			{
				v0 += (((v1 << 4) ^ (v1 >> 5)) + v1) ^ (sum + key[sum & 3]);
				sum += Delta;
				v1 += (((v0 << 4) ^ (v0 >> 5)) + v0) ^ (sum + key[(sum >> 11) & 3]);
			}

			data[0] = v0;
			data[1] = v1;
		}

		void __Decrypt64(uint32_t data[2]) const
		{
			std::uint32_t v0 = data[0];
			std::uint32_t v1 = data[1];
			std::uint32_t sum = (Delta * Cycles) & static_cast<std::uint32_t>(-1);
			const auto [k0, k1, k2, k3] = GetKey();
			const std::uint32_t key[]{ k0,k1,k2,k3 };
			
			for (std::uint32_t i = 0; i < Cycles; i++)
			{
				v1 -= (((v0 << 4) ^ (v0 >> 5)) + v0) ^ (sum + key[(sum >> 11) & 3]);
				sum -= Delta;
				v0 -= (((v1 << 4) ^ (v1 >> 5)) + v1) ^ (sum + key[sum & 3]);
			}
			
			data[0] = v0;
			data[1] = v1;
		}
	};

	template<std::uint32_t Cycles = 32, bool LittleEndian = true, std::uint32_t Delta = 0x9E3779B9>
	class Xxtea: ISymmetricAlgorithm<Xxtea<Cycles, LittleEndian, Delta>>
	{
	public:
		struct Key
		{
			std::uint32_t A, B, C, D;
		};

		Xxtea() = delete;

		explicit Xxtea(const Key key) : key(key) {}

		explicit Xxtea(const uint8_t keyBytes[16])
		{
			using __Detail::Bit::Endian;
			using __Detail::Bit::BSwap;

			key = Key
			{
				*(uint32_t*)&keyBytes[0 * 4],
				*(uint32_t*)&keyBytes[1 * 4],
				*(uint32_t*)&keyBytes[2 * 4],
				*(uint32_t*)&keyBytes[3 * 4]
			};

			if constexpr (LittleEndian && Endian::Native == Endian::Big ||
				!LittleEndian && Endian::Native == Endian::Little)
			{
				key.A = BSwap(key.A);
				key.B = BSwap(key.B);
				key.C = BSwap(key.C);
				key.D = BSwap(key.D);
			}
		}

		void Encrypt(std::uint8_t* data, std::uint64_t dataLen) const
		{
			if (dataLen % 8 != 0) throw std::runtime_error("Encrypt error: block sizes must be 64 bits");

			for (std::uint64_t n = dataLen / 8, i = 0; i < n; ++i)
			{
				Call64<true>(data + i * 8);
			}
		}

		template<typename ExecutionPolicy>
		void Encrypt(ExecutionPolicy&& policy, std::uint8_t* data, std::uint64_t dataLen) const
		{
			if (dataLen % 8 != 0) throw std::runtime_error("Encrypt error: block sizes must be 64 bits");

			__Detail::Enumerable::Range rng(dataLen / 8);
			std::for_each(policy, rng.begin(), rng.end(), [&](const auto i)
				{
					Call64<true>(data + i * 8);
				});
		}

		void Decrypt(std::uint8_t* data, std::uint64_t dataLen) const
		{
			if (dataLen % 8 != 0) throw std::runtime_error("Decrypt error: block sizes must be 64 bits");

			for (std::uint64_t n = dataLen / 8, i = 0; i < n; ++i)
			{
				Call64<false>(data + i * 8);
			}
		}

		template<typename ExecutionPolicy>
		void Decrypt(ExecutionPolicy&& policy, std::uint8_t* data, std::uint64_t dataLen) const
		{
			if (dataLen % 8 != 0) throw std::runtime_error("Encrypt error: block sizes must be 64 bits");

			__Detail::Enumerable::Range rng(dataLen / 8);
			std::for_each(policy, rng.begin(), rng.end(), [&](const auto i)
				{
					Call64<false>(data + i * 8);
				});
		}

		const Key& GetKey() const
		{
			return key;
		}

	private:
		Key key;

	protected:
		void Encrypt64(std::uint32_t data[2]) const
		{
			//static_cast<const T*>(this)->__Encrypt64(data);
		}

		void Decrypt64(uint32_t data[2]) const
		{
			//static_cast<const T*>(this)->__Decrypt64(data);
		}

		template<bool Enc>
		void Call64(std::uint8_t data[8]) const
		{
			using __Detail::Bit::Endian;
			using __Detail::Bit::BSwap;

			std::uint32_t dt[2]
			{
				*(uint32_t*)&data[0 * 4],
				*(uint32_t*)&data[1 * 4]
			};

			if constexpr ((LittleEndian && Endian::Native == Endian::Big) ||
				(!LittleEndian && Endian::Native == Endian::Little))
			{
				dt[0] = BSwap(dt[0]);
				dt[1] = BSwap(dt[1]);
			}

			if constexpr (Enc) Encrypt64(dt);
			else Decrypt64(dt);

			if constexpr (LittleEndian && Endian::Native == Endian::Big ||
				!LittleEndian && Endian::Native == Endian::Little)
			{
				dt[0] = BSwap(dt[0]);
				dt[1] = BSwap(dt[1]);
			}

			*(uint32_t*)&data[0 * 4] = dt[0];
			*(uint32_t*)&data[1 * 4] = dt[1];
		}
	};

	template<typename T>
	class IEncoding
	{
	public:
		std::vector<uint8_t> Encode(const std::vector<uint8_t>& data)
		{
			return static_cast<T*>(this)->Encode(data);
		}

		std::vector<uint8_t> Decode(const std::vector<uint8_t>& data)
		{
			return static_cast<T*>(this)->Decode(data);
		}
	};

	class Base85 : IEncoding<Base85>
	{
	public:
		std::vector<uint8_t> Encode(const std::vector<uint8_t>& data);

		std::vector<uint8_t> Decode(const std::vector<uint8_t>& data);
	};

	class Path85 : IEncoding<Path85>
	{
	public:
		std::vector<uint8_t> Encode(const std::vector<uint8_t>& data);

		std::vector<uint8_t> Decode(const std::vector<uint8_t>& data);
	};
}
