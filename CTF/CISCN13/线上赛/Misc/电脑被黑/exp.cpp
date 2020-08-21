#include <cstdio>

int main(int argc, char* argv[])
{
	const auto path = R"(Z:\fakeflag.txt)";
    char v4 = 34;
    char v5 = 0;
    const auto v7 = fopen(path, "rb");
	const auto stream = fopen(path, "rb+");
    while (true)
    {
	    const char v6 = fgetc(v7);
        if (v6 == -1)
            break;
        // fputc(v4 ^ (v5 + v6), stream);
        fputc((v4 ^ v6) - v5, stream);
        v4 += 34;
        v5 = v5 + 2 & 0xF;
    	
    }
    fclose(v7);
    fclose(stream);
}
