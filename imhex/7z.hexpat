struct StartHeader
{
	u8 Sign[6] [[name("Signature")]];
	u8 Ver[2] [[name("Format version")]];
	u32 Crc [[name("CRC of the following 20 bytes")]];
	u64 EndHeaderOffset [[name("relative offset of End Header")]];
	u64 EndHeaderLength [[name("the length of End Header")]];
	u32 EndHeaderCrc [[name("CRC of the End Header")]];
} [[static]];

StartHeader startHeader @ 0x00 [[name("Start of archive")]];

struct EndHeader
{
	u8 Val [[name("flag")]];
	if (Val == 0x01)
	{
		char Comment[startHeader.EndHeaderLength - sizeof(Val)] [[name("Comment")]];
	}
};

EndHeader endHeader @ (sizeof(startHeader)  + startHeader.EndHeaderOffset) [[name("End of archive")]];
