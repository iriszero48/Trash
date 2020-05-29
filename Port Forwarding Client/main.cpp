#include <cstdio>
#include <cstdlib>
#include <ctime>
#include <cstdint>
#include <thread>

#define _ToString(x) #x
#define ToString(x) _ToString(x)
#define Line ToString(__LINE__)

#if (_WIN32 || _WIN64)

#include <WinSock2.h>
#include <WS2tcpip.h>
#include <dos.h>

#pragma comment(lib, "ws2_32.lib")
#pragma comment(lib, "User32.lib")

#define _CRT_SECURE_NO_WARNINGS
#pragma warning(disable : 4996)

#define CloseSocket closesocket


#define err(status, ...)\
{\
	fprintf(stderr, __VA_ARGS__);\
	exit(status);\
}

#else

#include <err.h>

#define CloseSocket close

#endif

static uint64_t recvCount, sendCount;

inline void SleepSeconds(uint32_t seconds)
{
#if (_WIN32 || _WIN64)
    Sleep(seconds * 1000);
#else
    sleep(seconds);
#endif
}

void TimePrinter()
{
    time_t t = time(NULL);
    struct tm* lt = localtime(&t);
    fflush(stdout);
    printf("[ %d-%d-%d %d:%d:%d ] ", lt->tm_year + 1900, lt->tm_mon + 1, lt->tm_mday, lt->tm_hour, lt->tm_min,
        lt->tm_sec);
}

void Transfer(SOCKET sock1, SOCKET sock2, bool isRecv = true)
{
    char buf[4096] = { 0 };
    int len;
    while ((len = recv(sock1, buf, 4096, 0)) > 0)
    {
        send(sock2, buf, len, 0);
        if (isRecv) recvCount += len;
        else sendCount += len;
    }
}

int main(int argc, char* argv[])
{
    if (argc != 4) err(EXIT_FAILURE, "Usage:\n    %s ServerIp ServerPort ClientPort.\n", argv[0]);
    const char* serverIp = argv[1];
    const char* serverPort = argv[2];
    const char* clientPort = argv[3];
    TimePrinter();
    printf("init...\n");

#if (_WIN32 || _WIN64)
    WSADATA wsaData;
    if (WSAStartup(MAKEWORD(2, 2), &wsaData) < 0) err(EXIT_FAILURE, Line ":WinSock init fail");
#else
    struct sigaction action;
    action.sa_handler = [](int) {};
    sigemptyset(&action.sa_mask);
    action.sa_flags = 0;
    sigaction(SIGPIPE, &action, nullptr);
#endif
	
    struct addrinfo hints;
    memset(&hints, 0, sizeof(struct addrinfo));
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;

    std::thread logThread([]()
    {
            while (true)
            {
                fflush(stdout);
                printf("recv:%.fkB - send:%.fkB\r\033[k", round(recvCount / 1024.), round(sendCount / 1024.));
                SleepSeconds(1);
            }
    });

    while (true)
    {
	    try
	    {
            TimePrinter();
            printf("Link start...\n");

            struct addrinfo* serverAddr;
            if (getaddrinfo(serverIp, serverPort, &hints, &serverAddr) != 0) err(EXIT_FAILURE, Line ":(server)getaddrinfo(%s,%s) failed.\n", serverIp, serverPort);
            SOCKET serverSock = socket(serverAddr->ai_family, serverAddr->ai_socktype, serverAddr->ai_protocol);
            if (serverSock <= 0) err(EXIT_FAILURE, Line":(server)Can't open socket");

            struct addrinfo* clientAddr;
            if (getaddrinfo("localhost", clientPort, &hints, &clientAddr) != 0) err(EXIT_FAILURE, Line ":(client)getaddrinfo(%s,%s) failed.\n", "localhost", clientPort);
            SOCKET clientSock = socket(clientAddr->ai_family, clientAddr->ai_socktype, clientAddr->ai_protocol);
            if (clientSock <= 0) err(EXIT_FAILURE, Line":(server)Can't open socket");

            if (connect(serverSock, serverAddr->ai_addr, serverAddr->ai_addrlen) != 0)
            {
                perror(Line ":(server)connect() failed.");
                CloseSocket(serverSock);
                SleepSeconds(1);
                continue;
            }
            if (connect(clientSock, clientAddr->ai_addr, clientAddr->ai_addrlen) != 0)
            {
                perror(Line ":(client)connect() failed.");
                CloseSocket(clientSock);
                SleepSeconds(1);
                continue;
            }

            std::thread thread1(Transfer, clientSock, serverSock, false);
            std::thread thread2(Transfer, serverSock, clientSock, true);

            thread2.join();

            CloseSocket(clientSock);
            CloseSocket(serverSock);
	    }
	    catch (std::exception& e)
	    {
            perror(e.what());
	    }
    }
}
