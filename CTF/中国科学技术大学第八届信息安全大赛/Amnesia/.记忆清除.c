const char my_interp[] __attribute__((section(".interp"))) = __FILE__;
//extern void _start(void) __attribute__((section("load")));

int main() {
	
}

void start(void) __attribute__((constructor (101))); 
__attribute__((section ("data"))) void start() {
	const char* s = "H";
	__asm__("mov %edx, 1");
	__asm__("mov %ecx, %0" : "=r" (s));
	__asm__("mov %ebx, 1");
	__asm__("mov %eax, 4");
	__asm__("int $0x80");
}
