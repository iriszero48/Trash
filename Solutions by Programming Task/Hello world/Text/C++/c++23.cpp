// 2024/06

// clang++-17 -std=c++23 -xc++-system-header --precompile print -o print.pcm
// clang++-17 -std=c++23 -fmodule-file=print.pcm c++23.cpp
import <print>;

int main()
{
  std::println("Hello world!");
}
