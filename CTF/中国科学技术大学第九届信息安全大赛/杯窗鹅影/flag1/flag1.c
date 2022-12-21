#include <stdio.h>

int main()
{
    const char* path = "/flag1";

    FILE* fs = fopen(path, "r");
    char buf[4096] = { 0 };
    fread(buf, 4096, sizeof(char), fs);
    puts(buf);
    fclose(fs);
}