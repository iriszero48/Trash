#include "IceKey.H"

using namespace System;
using namespace Text;
using namespace IO;
using namespace Security::Cryptography;
using namespace Collections::Generic;

int main()
{
	auto ice = IceKey(0);

	//hex字符串转byte
	auto input = gcnew array<unsigned char>
	{
		0x3A, 0xCF, 0x8D, 0x62, 0xAA,
			0xA0, 0xB6, 0x30, 0xC4, 0xAF,
			0x43, 0xAF, 0x32, 0x7C, 0xE1,
			0x29, 0xD4, 0x6F, 0x0F, 0xEB,
			0x98, 0xD9, 0x04, 0x0F, 0x71,
			0x3B, 0xE6, 0x55, 0x02, 0xA5,
			0x10, 0x7A
	};
	const auto len = input->Length;
	const pin_ptr<unsigned char> inputPtr = &input[0];
	auto output = gcnew array<unsigned char>(len);
	const pin_ptr<unsigned char> outputPtr = &output[0];

	//求key, "iriszero"的md5的字符串
	const pin_ptr<unsigned char> keyPtr = &Encoding::ASCII->GetBytes(
		BitConverter::ToString(
			MD5CryptoServiceProvider().ComputeHash(
				Encoding::ASCII->GetBytes(
					"iriszero")))->Replace("-", "")->ToLower())[0];
	ice.set(keyPtr);
	auto list = gcnew List<int>();

	//decrypt部分
	for (auto i = 0; i < len; i = i + 8) ice.decrypt(&inputPtr[i], &outputPtr[i]);
	Console::WriteLine(Encoding::ASCII->GetString(output));
	Console::ReadLine();
}
