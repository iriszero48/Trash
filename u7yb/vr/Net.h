#pragma once

#include <stdexcept>
#include <optional>
#include <string>
#include <string_view>

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

namespace Net
{
	namespace __Detail
	{
		template<typename Str, typename...Args>
		void Append(Str& str, Args&&... args)
		{
			(str.append(args), ...);
		}

		template<typename...Args>
		std::string AppendNew(Args&&... args)
		{
			std::string buf;
			(buf.append(args), ...);
			return buf;
		}
	}

	class Exception : public std::runtime_error
	{
	public:
		template<typename...Args>
		Exception(Args&&... str): std::runtime_error(__Detail::AppendNew(std::forward<Args>(str)...)){}
	};

#define ThrowExcpet(...) throw Exception("[", funcName, "] ", __VA_ARGS__)

	namespace __Detail
	{
		enum Platform
		{
			Windows = 0,
			Linux = 1,
			Macintosh = 2,
			Native =
#if defined(WIN32) || defined(_WIN32) || defined(__WIN32__) || defined(__NT__) || defined(_WIN64)
				Windows
#define WindowsPlatform
#elif __linux__
				Linux
#define LinuxPlatform
#elif __APPLE__
				Macintosh
#define MacintoshPlatform
#else
#error "Unknown Platform"
#endif
		};

		using SocketType =
#ifdef WindowsPlatform
			SOCKET;
#else
			int;
#endif

		std::string GetIpFromURL(const std::string& url)
		{
			constexpr auto funcName = "Net::__Detail::GetIpFromURL";

			struct addrinfo hints, *res;
			int error;
			char host[NI_MAXHOST]{0};
			memset(&hints, 0, sizeof hints);
			hints.ai_family = PF_UNSPEC;
			hints.ai_socktype = SOCK_DGRAM;

			error = getaddrinfo(url.c_str(), nullptr, &hints, &res);
			if (error) ThrowExcpet("getaddrinfo: ", gai_strerror(error));
			if (!res) ThrowExcpet("getaddrinfo: NULL");

			error = getnameinfo(res->ai_addr, res->ai_addrlen, host, sizeof host, nullptr, 0, NI_NUMERICHOST);
			if (error) ThrowExcpet("getnameinfo:", gai_strerror(error));

			freeaddrinfo(res);
			return host;
		}
	}

	class TcpClient
	{
		sockaddr_in addrInfo{};
		__Detail::SocketType sock{};

	public:
		TcpClient(const std::string& host, const uint16_t port)
		{
			constexpr auto funcName = "Net::TcpClient::TcpClient";

#ifdef WindowsPlatform
			WSADATA wsaData;
        	if (WSAStartup(MAKEWORD(2, 2), &wsaData) < 0) throw std::std::runtime_error(ExceptPrefix "WinSock init fail");
#else
			struct sigaction action;
			action.sa_handler = [](int) {};
			sigemptyset(&action.sa_mask);
			action.sa_flags = 0;
			sigaction(SIGPIPE, &action, nullptr);
#endif
			addrInfo.sin_family = AF_INET;
			addrInfo.sin_port = htons(port);

			const auto ip = __Detail::GetIpFromURL(host);
			inet_pton(AF_INET, ip.c_str(), &addrInfo.sin_addr);
			sock = socket(PF_INET, SOCK_STREAM, 0);
			if (sock <= 0) ThrowExcpet("Can't open socket");

			const auto ret = connect(sock, (sockaddr*)&addrInfo, sizeof addrInfo);
			if (ret) ThrowExcpet("connect: ", std::to_string(ret));
		}

		~TcpClient()
		{
			Close();
		}

		void Close()
		{
			#ifdef WindowsPlatform
				closesocket(sock);
			#else
				close(sock);
			#endif
		}

		int64_t Send(const std::string_view& buf)
		{
			constexpr auto funcName = "Net::TcpClient::Send";

			const auto ret = send(sock, buf.data(), buf.length(), 0);
			if (ret < 0) ThrowExcpet("send: ", std::to_string(ret));
			return ret;
		}

		int64_t Recv(char* buf, const uint64_t size)
		{
			constexpr auto funcName = "Net::TcpClient::Recv";

			const auto ret = recv(sock, buf, size, 0);
			if (ret < 0) ThrowExcpet("recv: ", std::to_string(ret));
			return ret;
		}
	};
}
