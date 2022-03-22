#include <cstdint>

#include <emscripten.h>

extern "C" {
EMSCRIPTEN_KEEPALIVE
const char* GenMd5(uint8_t *buff, const int buffLength)
{
    return "test";
}
}