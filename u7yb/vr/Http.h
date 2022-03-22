#pragma once

#include <unordered_map>
#include <charconv>
#include <optional>
#include <string>
#include <string_view>
#include <iostream>

#include "Net.h"

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

#define Enum(option, ...)\
	enum class option { __VA_ARGS__ };\
	static auto __##option##_map__ = (([i = 0](std::string str) mutable\
	{\
		str.erase(std::remove(str.begin(), str.end(), ' '), str.end());\
		std::unordered_map<option, const std::string> res{};\
		std::string item;\
		std::stringstream stringStream(str);\
		while (std::getline(stringStream, item, ',')) res.emplace(static_cast<option>(i++), item);\
		return res;\
	})(#__VA_ARGS__));\
	inline std::string EnumToString(const option& in) { return __##option##_map__.at(in); }\
	inline std::optional<option> To##option(const std::string& in)\
	{\
		for (const auto& [k, v] : __##option##_map__) if (v == in) return k; return std::nullopt;\
	}

namespace HTTP
{
	namespace __Detail
	{
		template <typename T>
		T ReadInt(const std::string_view& buf)
		{
			T val{};
			std::from_chars(buf.data(), buf.data() + buf.length(), val);
			return val;
		}

		template <typename T>
		std::string IntToString(const T& num, const int base = 10)
		{
			std::string buf(65, '\0');
			std::to_chars(buf.data(), buf.data() + buf.length(), num, base);
			return buf;
		}

		std::string_view Trim(const std::string_view& str)
		{
			const auto start = std::find_if(str.begin(),  str.end(), [](const auto c){ return c != ' '; });
			const auto subStr = str.substr(std::distance(str.begin(), start));
			const auto end = std::find_if(subStr.rbegin(), subStr.rend(), [](const auto c){ return c != ' '; });
			return subStr.substr(0, subStr.length() - std::distance(end, subStr.rbegin()));
		}

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

		std::string UrlEncode(const std::string& raw)
		{
			std::string res{};
			auto i = std::string::npos;
			while (true)
			{
				i++;
				auto beg = raw.begin();
				std::advance(beg, i);
				auto pos = std::find_if(beg, raw.end(), [](const auto c)
				{
					static const auto UrlEncodeTable = []()
					{
						std::array<bool, 256> tab{};
						for (auto i = 0; i < 256; ++i)
						{
							tab[i] = !(isalnum(i) || i == '*' || i == '-' || i == '.' || i == '_');
						}
						return tab;
					}();
					return UrlEncodeTable[c];
				});
				if (pos == raw.end())
				{
					res.append(raw, i);
					break;
				}
				const auto dis = std::distance(beg, pos);
				res.append(raw, i, dis);
				res.append("%");
				res.append(IntToString((uint8_t)*pos, 16));
				i += dis;
			}
			return res;
		}
	}

	Enum(HttpVersion, V1_1)
	Enum(HttpMethod, GET, HEAD, POST, PUT, DELETE, CONNECT, OPTIONS, TRACE, PATCH)

	static const std::unordered_map<uint16_t, std::string_view> HttpStatusString
			{
					{100, "Continue"},
					{101, "Switching Protocol"},
					{103, "Early Hints"},
					{200, "OK"},
					{201, "Created"},
					{202, "Accepted"},
					{203, "Non - Authoritative Information"},
					{204, "No Content"},
					{205, "Reset Content"},
					{206, "Partial Content"},
					{300, "Multiple Choices"},
					{301, "Moved Permanently"},
					{302, "Found"},
					{303, "See Other"},
					{304, "Not Modified"},
					{307, "Temporary Redirect"},
					{308, "Permanent Redirect"},
					{400, "Bad Request"},
					{401, "Unauthorized"},
					{402, "Payment Required"},
					{403, "Forbidden"},
					{404, "Not Found"},
					{405, "Method Not Allowed"},
					{406, "Not Acceptable"},
					{407, "Proxy Authentication Required"},
					{408, "Request Timeout"},
					{409, "Conflict"},
					{410, "Gone"},
					{411, "Length Required"},
					{412, "Precondition Failed"},
					{413, "Payload Too Large"},
					{414, "URI Too Long"},
					{415, "Unsupported Media Type"},
					{416, "Range Not Satisfiable"},
					{417, "Expectation Failed"},
					{418, "I'm a teapot"},
					{422, "Unprocessable Entity"},
					{425, "Too Early"},
					{426, "Upgrade Required"},
					{428, "Precondition Required"},
					{429, "Too Many Requests"},
					{431, "Request Header Fields Too Large"},
					{451, "Unavailable For Legal Reasons"},
					{500, "Internal Server Error"},
					{501, "Not Implemented"},
					{502, "Bad Gateway"},
					{503, "Service Unavailable"},
					{504, "Gateway Timeout"},
					{505, "HTTP Version Not Supported"},
					{506, "Variant Also Negotiates"},
					{507, "Insufficient Storage"},
					{508, "Loop Detected"},
					{510, "Not Extended"},
					{511, "Network Authentication Required"},
			};

	std::string HttpVersionToString(const HttpVersion& ver)
	{
		auto buf = EnumToString(ver);
		std::transform(buf.begin(),  buf.end(), buf.begin(), [](const auto c)
		{
			if (c == 'V') return '/';
			if (c == '_') return '.';
			return c;
		});
		buf.insert(0, "HTTP");
		return buf;
	}

	class Exception : public std::runtime_error
	{
	public:
		template<typename...Args>
		Exception(Args&&... str): std::runtime_error(__Detail::AppendNew(std::forward<Args>(str)...)){}
	};

	struct HttpRequest
	{
		HttpMethod Method;
		std::string Url;
		HttpVersion Version;
		std::unordered_map<std::string, std::string> Headers;
		std::string Content;

		[[nodiscard]] std::string ToString() const
		{
			std::string buf;
			__Detail::Append(buf, EnumToString(Method), " ", Url, " ", HttpVersionToString(Version), "\r\n");
			for (const auto& [k, v]: Headers) __Detail::Append(buf, k, ": ", v, "\r\n");
			__Detail::Append(buf, "\r\n");
			__Detail::Append(buf, Content);
			return buf;
		}
	};

	struct HttpResponse
	{
		HttpVersion Version;
		uint16_t StatusCode;
		std::unordered_map<std::string, std::string> Headers;
		std::string Content;

		static HttpResponse Parse(const std::string_view& buf)
		{
			constexpr auto funcName = "HTTP::HttpResponse::Parse";

			HttpResponse res{};

			std::string_view::size_type pos;
			auto cur = buf.find('/');
			if (const auto head = buf.substr(0, cur); head != "HTTP")
				ThrowExcpet("Invalid Head: ", head);

			pos = cur + 1;
			cur = buf.find(' ', pos);
			const auto ver = buf.substr(cur, cur - pos);
			if (ver == "1.1") res.Version = HttpVersion::V1_1;

			pos = cur + 1;
			cur = buf.find(' ', pos);
			const auto code = __Detail::ReadInt<decltype(StatusCode)>(buf.substr(pos, cur - pos));
			res.StatusCode = code;

			pos = buf.find('\n') + 1;
			while (true)
			{
				cur = buf.find("\r\n", pos);
				const auto header = buf.substr(pos, cur - pos);
				if (header.empty())
				{
					pos = cur + 2;
					break;
				}

				const auto splitPos = header.find(':');
				const auto k = std::string(__Detail::Trim(header.substr(0, splitPos)));
				const auto v = std::string(__Detail::Trim(header.substr(splitPos + 1)));
				res.Headers[k] = v;
				pos = cur + 2;
			}

			res.Content = buf.substr(pos);

			return res;
		}
	};

	class HttpClient
	{
	public:
		HttpClient() {};

		HttpResponse Get(const std::string& url, std::unordered_map<std::string, std::string> headers = {})
		{
			constexpr auto funcName = "HTTP::HttpClient::Get";

			constexpr auto urlHeadSize = 7; // "http://"
			if (url.substr(0, urlHeadSize) != "http://")
				throw Exception("[", funcName, "] ", "Invalid url");

			const auto subUrl = url.substr(7);
			const auto pos = subUrl.find('/');
			const auto host = subUrl.substr(0, pos);
			const auto portPos = host.find(':');

			uint16_t port = 80;
			if (portPos != decltype(subUrl)::npos)
				port = __Detail::ReadInt<uint16_t>(host.substr(portPos + 1));

			Net::TcpClient tcp(host.substr(0, portPos), port);

			HttpRequest req{};
			req.Url = subUrl.substr(pos);
			req.Headers["Host"] = host;
			for (const auto& [k, v]: headers) req.Headers[k] = v;
			req.Method = HttpMethod::GET;
			req.Version = HttpVersion::V1_1;
			//puts(req.ToString().c_str());

			tcp.Send(req.ToString());

			constexpr auto bufSize = 8192;
			char buf[bufSize]{};
			const auto ret = tcp.Recv(buf, bufSize);
			//puts(buf);
			return HttpResponse::Parse(std::string_view(buf, ret));
		}
	};
}
