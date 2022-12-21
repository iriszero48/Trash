struct file_head
{
	char sign[8];
	be u32 ver;
	char min_ver[6];
	char file_ver[12];
	be u64 arch_size;
	be u32  compressed_data_size [[name("compressed 'info block' data size")]];
	be u32  uncompressed_data_size;
	be u32 flags;
};

file_head header @ 0;

struct info_block
{
	u8 data[header.compressed_data_size] [[name("lz4")]];
 };

info_block info @ sizeof(header);
