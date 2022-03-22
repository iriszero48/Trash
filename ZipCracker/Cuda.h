#pragma once

#include <cstdint>
#include <string>

void FuckZipCrcCuda(uint8_t* flags, const uint8_t* fileHead, const std::string& password, const std::string& alphabet, uint64_t chunkSize);