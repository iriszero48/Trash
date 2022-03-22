#include <iostream>

#include "Path85.h"

int main(int, char**)
{
    std::cout << Path85::__Detail::Table << std::endl;

    const auto data = "iriszero";
    const auto enc = Path85::Encode(data);
    std::cout << enc << std::endl;
    const auto dec = Path85::Decode(enc);
    std::cout << dec << std::endl;

    const auto y = [](const auto f) {
			return [](const auto x) { return x(x); }(
				[=](const auto y) -> std::function <int(int)> {
					return f([=](const auto a) {
						return y(y) (a);
					});
			});
		};

		const auto fib = y([](const auto f) {
			return [=](const auto n) {
				return n < 2 ? n : f(n - 1) + f(n - 2);
			};
		});

        const auto v = fib(10);
		std::cout << v;
}


 int gf_mult(int a, int b, int m)
{
int res = 0;

            while (b != 0)
            {
                if ((b & 1) != 0)
                    res ^= a;

                a <<= 1;
                b >>= 1;

                if (a >= 256)
                    a ^= m;
            }

    return (res);
}