#include <cstdio>
#include <WinSock2.h>
#include <WS2tcpip.h>


#pragma comment(lib, "ws2_32.lib")
#pragma comment(lib, "User32.lib")

int main(int argc, char* argv[])
{
	WSADATA wsaData;
	if (WSAStartup(MAKEWORD(2, 2), &wsaData) < 0);
	int fd = socket(AF_INET, SOCK_STREAM, 0);
	struct sockaddr_in addr;
	addr.sin_family = AF_INET;
	addr.sin_port = 0;
	inet_pton(AF_INET,"202.38.93.111", &addr.sin_addr);
	if (connect(fd, (const struct sockaddr*) & addr, sizeof(addr)) == -1);
	char buf[4096]{ 0 };
	recv(fd, buf, 4096, 0);
	puts(buf);
}
