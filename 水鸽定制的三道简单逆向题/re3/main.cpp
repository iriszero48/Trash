class IceSubkey
{
public:
	unsigned long val[3];
};

static unsigned long ice_sbox[4][1024];
static int ice_sboxes_initialised = 0;

static const int ice_smod[4][4] = {
	{333, 313, 505, 369},
	{379, 375, 319, 391},
	{361, 445, 451, 397},
	{397, 425, 395, 505}
};

static const int ice_sxor[4][4] = {
	{0x83, 0x85, 0x9b, 0xcd},
	{0xcc, 0xa7, 0xad, 0x41},
	{0x4b, 0x2e, 0xd4, 0x33},
	{0xea, 0xcb, 0x2e, 0x04}
};

static const unsigned long ice_pbox[32] = {
	0x00000001, 0x00000080, 0x00000400, 0x00002000,
	0x00080000, 0x00200000, 0x01000000, 0x40000000,
	0x00000008, 0x00000020, 0x00000100, 0x00004000,
	0x00010000, 0x00800000, 0x04000000, 0x20000000,
	0x00000004, 0x00000010, 0x00000200, 0x00008000,
	0x00020000, 0x00400000, 0x08000000, 0x10000000,
	0x00000002, 0x00000040, 0x00000800, 0x00001000,
	0x00040000, 0x00100000, 0x02000000, 0x80000000
};

static const int ice_keyrot[16] = {
	0, 1, 2, 3, 2, 1, 3, 0,
	1, 3, 2, 0, 3, 1, 0, 2
};

static unsigned int gf_mult(unsigned int a, unsigned int b, unsigned int m)
{
	unsigned int res = 0;
	while (b)
	{
		if (b & 1)
			res ^= a;
		a <<= 1;
		b >>= 1;
		if (a >= 256)
			a ^= m;
	}
	return res;
}

static unsigned long gf_exp7(const unsigned int b, const unsigned int m)
{
	if (b == 0)
		return (0);
	auto x = gf_mult(b, b, m);
	x = gf_mult(b, x, m);
	x = gf_mult(x, x, m);
	return (gf_mult(b, x, m));
}

static unsigned long IcePerm32(unsigned long x)
{
	unsigned long res = 0;
	auto pbox = ice_pbox;
	while (x)
	{
		if (x & 1)
			res |= *pbox;
		pbox++;
		x >>= 1;
	}
	return res;
}

static void ice_sboxes_init()
{
	for (auto i = 0; i < 1024; i++)
	{
		const auto col = (i >> 1) & 0xff;
		const auto row = (i & 0x1) | ((i & 0x200) >> 8);
		auto x = gf_exp7(col ^ ice_sxor[0][row], ice_smod[0][row]) << 24;
		ice_sbox[0][i] = IcePerm32(x);
		x = gf_exp7(col ^ ice_sxor[1][row], ice_smod[1][row]) << 16;
		ice_sbox[1][i] = IcePerm32(x);
		x = gf_exp7(col ^ ice_sxor[2][row], ice_smod[2][row]) << 8;
		ice_sbox[2][i] = IcePerm32(x);
		x = gf_exp7(col ^ ice_sxor[3][row], ice_smod[3][row]);
		ice_sbox[3][i] = IcePerm32(x);
	}
}

static unsigned long IceF(const unsigned long p, const IceSubkey* sk)
{
	const auto tl = p >> 16 & 0x3ff | (p >> 14 | p << 18) & 0xffc00;
	const auto tr = p & 0x3ff | p << 2 & 0xffc00;
	auto al = sk->val[2] & (tl ^ tr);
	auto ar = al ^ tr;
	al ^= tl;
	al ^= sk->val[0];
	ar ^= sk->val[1];
	return (ice_sbox[0][al >> 10] | ice_sbox[1][al & 0x3ff]
		| ice_sbox[2][ar >> 10] | ice_sbox[3][ar & 0x3ff]);
}

class IceKey
{
public:
	explicit IceKey(const int n)
	{
		if (!ice_sboxes_initialised)
		{
			ice_sboxes_init();
			ice_sboxes_initialised = 1;
		}
		if (n < 1)
		{
			_size = 1;
			_rounds = 8;
		}
		else
		{
			_size = n;
			_rounds = n * 16;
		}
		_keysched = new IceSubkey[_rounds];
	}

	~IceKey()
	{
		for (auto i = 0; i < _rounds; i++) for (auto j = 0; j < 3; j++) _keysched[i].val[j] = 0;
		_rounds = _size = 0;
		delete[] _keysched;
	}

	void Set(const unsigned char* key) const
	{
		int i;
		if (_rounds == 8)
		{
			unsigned short kb[4];
			for (i = 0; i < 4; i++)
				kb[3 - i] = (key[i * 2] << 8) | key[i * 2 + 1];
			ScheduleBuild(kb, 0, ice_keyrot);
			return;
		}
		for (i = 0; i < _size; i++)
		{
			unsigned short kb[4];
			for (auto j = 0; j < 4; j++) kb[3 - j] = key[i * 8 + j * 2] << 8 | key[i * 8 + j * 2 + 1];
			ScheduleBuild(kb, i * 8, ice_keyrot);
			ScheduleBuild(kb, _rounds - 8 - i * 8, &ice_keyrot[8]);
		}
	}

	void Encrypt(const unsigned char* ptext, unsigned char* ctext) const
	{
		int i;
		auto l = static_cast<unsigned long>(ptext[0]) << 24
			| static_cast<unsigned long>(ptext[1]) << 16
			| static_cast<unsigned long>(ptext[2]) << 8 | ptext[3];
		auto r = static_cast<unsigned long>(ptext[4]) << 24
			| static_cast<unsigned long>(ptext[5]) << 16
			| static_cast<unsigned long>(ptext[6]) << 8 | ptext[7];
		for (i = 0; i < _rounds; i += 2)
		{
			l ^= IceF(r, &_keysched[i]);
			r ^= IceF(l, &_keysched[i + 1]);
		}
		for (i = 0; i < 4; i++)
		{
			ctext[3 - i] = r & 0xff;
			ctext[7 - i] = l & 0xff;
			r >>= 8;
			l >>= 8;
		}
	}

	void Decrypt(const unsigned char* ctext, unsigned char* ptext) const
	{
		int i;
		auto l = static_cast<unsigned long>(ctext[0]) << 24
			| static_cast<unsigned long>(ctext[1]) << 16
			| static_cast<unsigned long>(ctext[2]) << 8 | ctext[3];
		auto r = (static_cast<unsigned long>(ctext[4]) << 24)
			| (static_cast<unsigned long>(ctext[5]) << 16)
			| (static_cast<unsigned long>(ctext[6]) << 8) | ctext[7];
		for (i = _rounds - 1; i > 0; i -= 2)
		{
			l ^= IceF(r, &_keysched[i]);
			r ^= IceF(l, &_keysched[i - 1]);
		}
		for (i = 0; i < 4; i++)
		{
			ptext[3 - i] = r & 0xff;
			ptext[7 - i] = l & 0xff;
			r >>= 8;
			l >>= 8;
		}
	}

	int KeySize() const
	{
		return (_size * 8);
	}

	static int BlockSize()
	{
		return 8;
	}

private:
	void ScheduleBuild(unsigned short* kb, const int n, const int* keyrot) const
	{
		for (auto i = 0; i < 8; i++)
		{
			int j;
			const auto kr = keyrot[i];
			auto isk = &_keysched[n + i];
			for (j = 0; j < 3; j++) isk->val[j] = 0;
			for (j = 0; j < 15; j++)
			{
				const auto currySk = &isk->val[j % 3];
				for (auto k = 0; k < 4; k++)
				{
					const auto curryKb = &kb[kr + k & 3];
					const auto bit = *curryKb & 1;
					*currySk = *currySk << 1 | bit;
					*curryKb = *curryKb >> 1 | (bit ^ 1) << 15;
				}
			}
		}
	}

	int _size;
	int _rounds;
	IceSubkey* _keysched;
};

#include <iostream>

using namespace System;
using namespace Text;
using namespace IO;
using namespace Security::Cryptography;
using namespace Collections::Generic;

int main()
{
	String^ flag = "3ACF8D62AAA0B630C4AF43AF327CE129D46F0FEB98D9040F713BE65502A5107A";
	std::cout << "flag(format:xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx):";
	const auto ice = IceKey(0);
	auto input = Encoding::ASCII->GetBytes(Console::ReadLine());
	const auto len = input->Length;
	pin_ptr<unsigned char> inputPtr = &input[0];
	auto output = gcnew array<unsigned char>(len);
	pin_ptr<unsigned char> outputPtr = &output[0];
	pin_ptr<unsigned char> keyp = &Encoding::ASCII->GetBytes(
		BitConverter::ToString(
			MD5CryptoServiceProvider().ComputeHash(
				Encoding::ASCII->GetBytes(
					"iriszero")))->Replace("-", "")->ToLower())[0];
	ice.Set(keyp);
	for (auto i = 0; i < len; i += 8) ice.Encrypt(&inputPtr[i], &outputPtr[i]);
	std::cout << (BitConverter::ToString(output)->Replace("-", "")->ToString() == flag ? "true" : "false") << std::endl;
	for (auto i = 0; i < len; i = i + 8) ice.Decrypt(&outputPtr[i], &inputPtr[i]);
}
