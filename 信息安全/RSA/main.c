#include <time.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
// windows库，用于生成高质量随机数
#include <Windows.h> 
#include <wincrypt.h>
// gmp库 用于大数字的计算
#include "gmp.h"

#define BigInteger mpz_t // gmp变量，用BigInteger取代
#define Let mpz_init_set_str // 初始化gmp变量并赋值
#define Val mpz_init // 初始化gmp变量
#define Del mpz_clear // 释放gmp变量
#define Dump(x) gmp_printf(#x" => %Zx\n\n", x) // 打印变量x
#define TipDump(n, x) gmp_printf(#n" => %Zx\n\n", x) // 使用其他名称打印变量x
// 打印变量并放在分界符中
#define Line(n) "------------------ "#n" ------------------\n"
#define KeyDump(n, a, b) gmp_printf(Line(n)"("#a", "#b") => (%Zx, %Zx)\n"Line(n)"\n", a, b)
// 随机生成素数
#define RandomPrime(n, gr, len) Val(n), mpz_urandomb(n, gr, len), mpz_nextprime(n, n)

int main(const int argc, char* argv[])
{
	if (argc != 4) fprintf(stderr, "%s p,q位长 e(hex) 明文(hex)", argv[0]), exit(EXIT_FAILURE);
	const uint32_t len = strtoul(argv[1], &argv[1], 10);
	// 初始化gmp的随机数生成器
	gmp_randstate_t gr;
	gmp_randinit_default(gr);
	// 使用wincrypt生成高质量随机数作为随机种子
	time_t rand = 0;
	HCRYPTPROV rnd;
	const LPCSTR userName = "iriszero";
	CryptAcquireContext(&rnd, userName, NULL, PROV_RSA_FULL, 0)
		? 0 : GetLastError() == NTE_BAD_KEYSET
		? CryptAcquireContext(&rnd, userName, NULL, PROV_RSA_FULL, CRYPT_NEWKEYSET) : 0;
	CryptGenRandom(rnd, sizeof(time_t), (BYTE*)&rand);
	CryptReleaseContext(rnd, 0);
	gmp_randseed_ui(gr, rand);
	BigInteger p, q, n, phiN, e, d, m, c, m0; // 初始化所需变量
	RandomPrime(p, gr, len), RandomPrime(q, gr, len), Dump(p), Dump(q); // 随机生成素数p,q
	Val(n), mpz_mul(n, p, q), Dump(n); // 求n
	Val(phiN), mpz_sub_ui(p, p, 1), mpz_sub_ui(q, q, 1), mpz_mul(phiN, p, q), TipDump(phi(n), phiN), Del(p), Del(q); // 求phi(n)
	// 求乘法逆元
	Let(e, argv[2], 16), Dump(e);
	Val(d), mpz_invert(d, e, phiN), Dump(d), Del(phiN);
	// 加密
	Let(m, argv[3], 16), TipDump(明文, m);
	Val(c), mpz_powm(c, m, e, n), TipDump(密文, c), Del(c), Del(m);
	Val(m0), mpz_powm(m0, c, d, n), TipDump(解密, m0), Del(m0), Del(c); // 解密
	KeyDump(公钥, e, n), KeyDump(私钥, d, n), Del(e), Del(n), Del(d); // 输出公钥和私钥
}
 
