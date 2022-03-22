#include <algorithm>
#include <filesystem>
#include <thread>
#include <chrono>
#include <fstream>
#include <unordered_map>
#include <atomic>
#include <map>
#include <execution>
#include <array>
#include <sstream>

#include <zip.h>

#include "Arguments.h"
#include "Convert.h"
#include "Log.h"
#include "StdIO.h"
#include "String.h"
#include "Time.h"
#include "Macro.h"
#include "File.h"
#include "CRC.h"
#include "Enumerable.h"

#ifdef _MSC_VER

#include <WinSock2.h>
#include <WS2tcpip.h>

#pragma comment(lib, "ws2_32.lib")
#pragma comment(lib, "User32.lib")

#define CudaEnable

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

#ifdef CudaEnable

#include "Cuda.h"

#endif

#if (defined _WIN32 || _WIN64)
#define windows
#endif

struct LoggerType
{
    // index 0
    using Time = decltype(std::chrono::system_clock::now());
    // index 1
    using ThreadId = decltype(std::this_thread::get_id());
    // index 2
    using File = std::string;
    // index 3
    using Line = std::string;
    // index 4
    using Msg = std::string;

    using AsTuple = std::tuple<Time, ThreadId, File, Line, Msg>;
};

static Logger<LoggerType::AsTuple> Log;

#define LogImpl(level, ...) Log.Write<level>(std::chrono::system_clock::now(), std::this_thread::get_id(), __FILE__, MacroLine, String::StringCombineNew(__VA_ARGS__))
#define LogError(...) LogImpl(LogLevel::Error, __VA_ARGS__)
#define LogWarn(...) LogImpl(LogLevel::Warn, __VA_ARGS__)
#define LogLog(...) LogImpl(LogLevel::Log, __VA_ARGS__)
#define LogInfo(...) LogImpl(LogLevel::Info, __VA_ARGS__)
#define LogDebug(...) LogImpl(LogLevel::Debug, __VA_ARGS__)

namespace Socket
{
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

            Option<const int> Recv(char* buf, const uint64_t bufSize) const
            {
                int len;
                if ((len = recv(socket, buf, bufSize, 0)) < 0)
                {
                    return {};
                }
                return { len };
            }

            Option<const int> Send(const char* buf, const uint64_t bufSize) const
            {
                int len;
                if ((len = send(socket, buf, bufSize, 0)) < 0)
                {
                    return {};
                }
                return { len };
            }

            void SendAll(const std::string& str) const
            {
                auto len = str.length();
                for (decltype(len) sent = 0; sent < len; sent += 4096)
                {
                    auto toSend = str.substr(sent, 4096);
                    Send(toSend.c_str(), toSend.length());
                }
            }

            std::string RecvAll() const
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
            if (bind(serverSocket, (struct sockaddr*)&serverSockAddr, sizeof serverSockAddr) < 0)
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
            client.socket = accept(serverSocket, (struct sockaddr*)&client.address, &client.addrLen);
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
}

// op[3] + len[17]

constexpr auto OpLen = 3;
constexpr auto NumLen = 17;
constexpr auto BufLen = 4096;
//to server    //to client
ArgumentOption(Op, fil, tab, req, fid, wrk, don, wit, acc)

template<typename ...T> struct ReqResVisitor : T... { using T::operator()...; };
template<typename ...T> ReqResVisitor(T...)->ReqResVisitor<T...>;

bool AddBaseN(uint8_t* c, const uint8_t* const a, const uint8_t* const b, const uint64_t n, const uint64_t base)
{
    int carry = 0;
    for (int i = n - 1; i >= 0; i--)
    {
        int curr = carry + a[i] + b[i];
        carry = curr / base;
        curr %= base;
        c[i] = curr;
    }
    //LogDebug("AddBaseN: a=[|",
    //    [&]() { std::string buf{}; for (auto i = 0; i < n; ++i) buf.append(Convert::ToString(a[i]) + ";"); return buf; }(),
    //    "|], b=[|",
    //    [&]() { std::string buf{}; for (auto i = 0; i < n; ++i) buf.append(Convert::ToString(b[i]) + ";"); return buf; }(),
    //    "|], n=", Convert::ToString(n), ", base=", Convert::ToString(base));
    //LogDebug("AddBaseN: c=[|", [&]() { std::string buf{}; for (auto i = 0; i < n; ++i) buf.append(Convert::ToString(c[i]) + ";"); return buf; }(), "|]");
    if (carry > 0) return false;
    return true;
}

bool AddUint64BaseN(uint8_t* c, const uint8_t* const a, uint64_t b, const uint64_t n, const uint64_t base)
{
    //LogDebug("AddUint64BaseN: a=[|",
    //    [&]() { std::string buf{}; for (auto i = 0; i < n; ++i) buf.append(Convert::ToString(a[i]) + ";"); return buf; }(),
    //    "|], b=",
    //    Convert::ToString(b), ", n=", Convert::ToString(n), ", base=", Convert::ToString(base));
    auto* nb = new uint8_t[n];
    for (int i = n - 1; i >= 0; --i)
    {
        nb[i] = b % base;
        b /= base;
    }
    //nb[0] = b;
    if (b > 0) return false;
    //LogDebug("AddUint64BaseN: nb=[|", [&]() { std::string buf{}; for (auto i = 0; i < n; ++i) buf.append(Convert::ToString(nb[i]) + ";"); return buf; }(), "|]");
    const auto succ = AddBaseN(c, a, nb, n, base);
    delete[] nb;
    return succ;
}

std::optional<std::string> PasswordAdd(const std::string& password, const uint64_t offset, const std::string& alphabet)
{
    std::string a(password.length(), '\0');
    std::string c(password.length(), '\0');
    std::transform(password.begin(), password.end(), a.begin(), [&](const char x) { return static_cast<char>(alphabet.find(x)); });
    const auto succ = AddUint64BaseN((uint8_t*)c.data(), (uint8_t*)a.data(), offset, password.length(), alphabet.length());
    if (!succ) return std::nullopt;
    std::transform(c.begin(), c.end(), c.begin(), [&](const char x) { return alphabet[x]; });
    //LogDebug("PasswordAdd: ", password, " + ", Convert::ToString(offset), " = ", c, " base ", alphabet);
    return { c };
}

static const auto mult_tab = []()
{
    std::array<uint8_t, 16384> res{};
    for (auto t = 0; t < 16384; t++) res[t] = ((t * 4 + 3) * (t * 4 + 2) >> 8) & 0xff;
    return res;
}();

static const uint32_t crc_32_tab[] =
{
    0x00000000UL, 0x77073096UL, 0xee0e612cUL, 0x990951baUL, 0x076dc419UL,
    0x706af48fUL, 0xe963a535UL, 0x9e6495a3UL, 0x0edb8832UL, 0x79dcb8a4UL,
    0xe0d5e91eUL, 0x97d2d988UL, 0x09b64c2bUL, 0x7eb17cbdUL, 0xe7b82d07UL,
    0x90bf1d91UL, 0x1db71064UL, 0x6ab020f2UL, 0xf3b97148UL, 0x84be41deUL,
    0x1adad47dUL, 0x6ddde4ebUL, 0xf4d4b551UL, 0x83d385c7UL, 0x136c9856UL,
    0x646ba8c0UL, 0xfd62f97aUL, 0x8a65c9ecUL, 0x14015c4fUL, 0x63066cd9UL,
    0xfa0f3d63UL, 0x8d080df5UL, 0x3b6e20c8UL, 0x4c69105eUL, 0xd56041e4UL,
    0xa2677172UL, 0x3c03e4d1UL, 0x4b04d447UL, 0xd20d85fdUL, 0xa50ab56bUL,
    0x35b5a8faUL, 0x42b2986cUL, 0xdbbbc9d6UL, 0xacbcf940UL, 0x32d86ce3UL,
    0x45df5c75UL, 0xdcd60dcfUL, 0xabd13d59UL, 0x26d930acUL, 0x51de003aUL,
    0xc8d75180UL, 0xbfd06116UL, 0x21b4f4b5UL, 0x56b3c423UL, 0xcfba9599UL,
    0xb8bda50fUL, 0x2802b89eUL, 0x5f058808UL, 0xc60cd9b2UL, 0xb10be924UL,
    0x2f6f7c87UL, 0x58684c11UL, 0xc1611dabUL, 0xb6662d3dUL, 0x76dc4190UL,
    0x01db7106UL, 0x98d220bcUL, 0xefd5102aUL, 0x71b18589UL, 0x06b6b51fUL,
    0x9fbfe4a5UL, 0xe8b8d433UL, 0x7807c9a2UL, 0x0f00f934UL, 0x9609a88eUL,
    0xe10e9818UL, 0x7f6a0dbbUL, 0x086d3d2dUL, 0x91646c97UL, 0xe6635c01UL,
    0x6b6b51f4UL, 0x1c6c6162UL, 0x856530d8UL, 0xf262004eUL, 0x6c0695edUL,
    0x1b01a57bUL, 0x8208f4c1UL, 0xf50fc457UL, 0x65b0d9c6UL, 0x12b7e950UL,
    0x8bbeb8eaUL, 0xfcb9887cUL, 0x62dd1ddfUL, 0x15da2d49UL, 0x8cd37cf3UL,
    0xfbd44c65UL, 0x4db26158UL, 0x3ab551ceUL, 0xa3bc0074UL, 0xd4bb30e2UL,
    0x4adfa541UL, 0x3dd895d7UL, 0xa4d1c46dUL, 0xd3d6f4fbUL, 0x4369e96aUL,
    0x346ed9fcUL, 0xad678846UL, 0xda60b8d0UL, 0x44042d73UL, 0x33031de5UL,
    0xaa0a4c5fUL, 0xdd0d7cc9UL, 0x5005713cUL, 0x270241aaUL, 0xbe0b1010UL,
    0xc90c2086UL, 0x5768b525UL, 0x206f85b3UL, 0xb966d409UL, 0xce61e49fUL,
    0x5edef90eUL, 0x29d9c998UL, 0xb0d09822UL, 0xc7d7a8b4UL, 0x59b33d17UL,
    0x2eb40d81UL, 0xb7bd5c3bUL, 0xc0ba6cadUL, 0xedb88320UL, 0x9abfb3b6UL,
    0x03b6e20cUL, 0x74b1d29aUL, 0xead54739UL, 0x9dd277afUL, 0x04db2615UL,
    0x73dc1683UL, 0xe3630b12UL, 0x94643b84UL, 0x0d6d6a3eUL, 0x7a6a5aa8UL,
    0xe40ecf0bUL, 0x9309ff9dUL, 0x0a00ae27UL, 0x7d079eb1UL, 0xf00f9344UL,
    0x8708a3d2UL, 0x1e01f268UL, 0x6906c2feUL, 0xf762575dUL, 0x806567cbUL,
    0x196c3671UL, 0x6e6b06e7UL, 0xfed41b76UL, 0x89d32be0UL, 0x10da7a5aUL,
    0x67dd4accUL, 0xf9b9df6fUL, 0x8ebeeff9UL, 0x17b7be43UL, 0x60b08ed5UL,
    0xd6d6a3e8UL, 0xa1d1937eUL, 0x38d8c2c4UL, 0x4fdff252UL, 0xd1bb67f1UL,
    0xa6bc5767UL, 0x3fb506ddUL, 0x48b2364bUL, 0xd80d2bdaUL, 0xaf0a1b4cUL,
    0x36034af6UL, 0x41047a60UL, 0xdf60efc3UL, 0xa867df55UL, 0x316e8eefUL,
    0x4669be79UL, 0xcb61b38cUL, 0xbc66831aUL, 0x256fd2a0UL, 0x5268e236UL,
    0xcc0c7795UL, 0xbb0b4703UL, 0x220216b9UL, 0x5505262fUL, 0xc5ba3bbeUL,
    0xb2bd0b28UL, 0x2bb45a92UL, 0x5cb36a04UL, 0xc2d7ffa7UL, 0xb5d0cf31UL,
    0x2cd99e8bUL, 0x5bdeae1dUL, 0x9b64c2b0UL, 0xec63f226UL, 0x756aa39cUL,
    0x026d930aUL, 0x9c0906a9UL, 0xeb0e363fUL, 0x72076785UL, 0x05005713UL,
    0x95bf4a82UL, 0xe2b87a14UL, 0x7bb12baeUL, 0x0cb61b38UL, 0x92d28e9bUL,
    0xe5d5be0dUL, 0x7cdcefb7UL, 0x0bdbdf21UL, 0x86d3d2d4UL, 0xf1d4e242UL,
    0x68ddb3f8UL, 0x1fda836eUL, 0x81be16cdUL, 0xf6b9265bUL, 0x6fb077e1UL,
    0x18b74777UL, 0x88085ae6UL, 0xff0f6a70UL, 0x66063bcaUL, 0x11010b5cUL,
    0x8f659effUL, 0xf862ae69UL, 0x616bffd3UL, 0x166ccf45UL, 0xa00ae278UL,
    0xd70dd2eeUL, 0x4e048354UL, 0x3903b3c2UL, 0xa7672661UL, 0xd06016f7UL,
    0x4969474dUL, 0x3e6e77dbUL, 0xaed16a4aUL, 0xd9d65adcUL, 0x40df0b66UL,
    0x37d83bf0UL, 0xa9bcae53UL, 0xdebb9ec5UL, 0x47b2cf7fUL, 0x30b5ffe9UL,
    0xbdbdf21cUL, 0xcabac28aUL, 0x53b39330UL, 0x24b4a3a6UL, 0xbad03605UL,
    0xcdd70693UL, 0x54de5729UL, 0x23d967bfUL, 0xb3667a2eUL, 0xc4614ab8UL,
    0x5d681b02UL, 0x2a6f2b94UL, 0xb40bbe37UL, 0xc30c8ea1UL, 0x5a05df1bUL,
    0x2d02ef8dUL
};

//#define crc32(crc,byte) (crc_32_tab[(uint8_t)(crc) ^ (uint8_t)(byte)] ^ ((crc) >> 8))
inline uint32_t crc32(const uint32_t crc, const uint8_t val)
{
    return crc_32_tab[static_cast<uint8_t>(crc) ^ val] ^ crc >> 8;
}

bool FuckZipCrc(const uint8_t* fileHead, const char* password, const uint64_t passwordLen)
{
    const auto* b = fileHead;
    uint32_t key0 = 0x12345678UL;
    uint32_t key1 = 0x23456789UL;
    uint32_t key2 = 0x34567890UL;
    for (auto i = 0; i < passwordLen; ++i)
    {
        key0 = crc32(key0, password[i]);
        key1 = (key1 + static_cast<uint8_t>(key0)) * 134775813 + 1;
        key2 = crc32(key2, key1 >> 24);
    }
    const auto* const e = b + 12 - 1;
    do
    {
        const uint8_t preTarget = *b++ ^ mult_tab[static_cast<uint16_t>(key2) >> 2];
        key0 = crc32(key0, preTarget);
        key1 = (key1 + static_cast<uint8_t>(key0)) * 134775813 + 1;
        key2 = crc32(key2, key1 >> 24);
    } while (b < e);
    const auto target = *b++ ^ mult_tab[static_cast<uint16_t>(key2) >> 2];
    return target == *b;
}

template<LogLevel LLevel>
bool FuckZip(const std::string& file, const std::string& password)
{
    zip_error_t error;
    zip_source_t* src = zip_source_buffer_create(file.data(), file.length(), 0, &error);
    if (src == NULL && error.zip_err != ZIP_ER_OK) { if constexpr (LLevel >= LogLevel::Debug) LogDebug("FuckZip: error: ", error.str); return false; }
    zip_t* za = zip_open_from_source(src, ZIP_RDONLY, &error);
    if (za == NULL && error.zip_err != ZIP_ER_OK) { if constexpr (LLevel >= LogLevel::Debug) LogDebug("FuckZip: error: ", Convert::ToString(error.zip_err)); return false; }
    //const auto filepath = String::FromStreamNew(std::this_thread::get_id(), std::hex) + ".zip";
    //File::WriteAllText(filepath, file);
    //int err = 0;
    //zip_t* za = zip_open(filepath.data(), 0, &err);
    //if (za == NULL) { zip_error_init_with_code(&error, err); LogDebug("FuckZip: error: ", Convert::ToString(error.zip_err)); return false; }
    zip_set_default_password(za, password.data());
    const auto entries = zip_get_num_entries(za, 0);
    if (entries < 0)
    {
        if constexpr (LLevel >= LogLevel::Debug) LogDebug("FuckZip: error: ", zip_get_error(za)->str);
        zip_close(za);
        return false;
    }
    for (int i = 0; i < entries; ++i)
    {
        struct zip_stat zs;
        if (zip_stat_index(za, i, 0, &zs) == 0)
        {
            const std::string_view filename(zs.name);
            if (filename[filename.length() - 1] != '/')
            {
                auto* const zf = zip_fopen_index(za, i, 0);
                if (zf == nullptr
                    && zip_get_error(za)->zip_err != ZIP_ER_OK)
                {
                    zip_close(za);
                    return false;
                }
                const auto buf = std::make_unique<uint8_t[]>(zs.size);
                if (zip_fread(zf, buf.get(), zs.size) < 0)
                {
                    zip_fclose(zf);
                    zip_close(za);
                    return false;
                }
                const std::string_view bufView(reinterpret_cast<char*>(buf.get()), zs.size);
                const auto crc = Crc::Crc32(bufView.begin(), bufView.end());
                if (crc != zs.crc)
                {
                    zip_fclose(zf);
                    zip_close(za);
                    if constexpr (LLevel >= LogLevel::Debug) LogDebug("FuckZip: crc: ", Convert::ToString(crc, 16), " <> ", Convert::ToString(zs.crc, 16));
                    return false;
                }
                zip_fclose(zf);
            }
        }
    }
    zip_close(za);
    return true;
}

struct ClientParams
{
    std::string Address;
    uint16_t Port;
    uint64_t ChunkSize;
    bool Cuda;
};

template<LogLevel LLevel>
void Client(const ClientParams& params)
{
    using std::chrono_literals::operator ""s;
    const auto reqData = [&](const Op& op, const std::string& dt)
    {
        Socket::Client client(params.Address, params.Port);
        client.Init();
        while (client.Connect().status == decltype(client.Connect())::Status::Some)
        {
            LogError("Unable to connect to server.");
            std::this_thread::sleep_for(10s);
        }
        client.SendAll(String::StringCombineNew(ToString(op), dt));
        if constexpr (LLevel >= LogLevel::Debug) LogDebug("Client<", ToString(LLevel), ">::reqData", ": send: ", ToString(op));
        const auto res = client.RecvAll();
        if constexpr (LLevel >= LogLevel::Debug) if (op != Op::fil) LogDebug("Client<", ToString(LLevel), ">::reqData", ": recv: ", res);
        client.Close();
        return res;
    };
    const auto req = [&](const Op& op)
    {
        return reqData(op, {});
    };
    const auto loadZip = [](const std::string& file, uint8_t* fileHead) -> uint32_t
    {
        std::stringstream f(file);
        f.write(file.data(), file.length());

        const auto fgetuint16 = [](decltype(f)& f) -> uint16_t
        {
            uint8_t buf[2] = { 0 };
            f.read(reinterpret_cast<char*>(buf), 2);
            return ((buf[0] << 0) | (buf[1] << 8));
        };

        const auto fgetuint32 = [](decltype(f)& f) -> uint32_t
        {
            uint8_t buf[4] = { 0 };
            f.read(reinterpret_cast<char*>(buf), 4);
            return ((buf[0] << 0) | (buf[1] << 8) | (buf[2] << 16) | (buf[3] << 24));
        };

        uint32_t maxUncomprSize = 0;

        while (!f.eof())
        {
            uint32_t id = fgetuint32(f);
            if (id == 0) break;
            if (id == 0x04034b50UL)
            {
                uint16_t version = fgetuint16(f);
                uint16_t flags = fgetuint16(f);
                uint16_t compression_method = fgetuint16(f);
                uint16_t lastmodtime = fgetuint16(f);
                uint16_t lastmoddate = fgetuint16(f);
                uint32_t crc32 = fgetuint32(f);
                uint32_t compr_size = fgetuint32(f);
                if (compr_size > maxUncomprSize) maxUncomprSize = compr_size;
                uint32_t uncompr_size = fgetuint32(f);
                uint16_t name_len = fgetuint16(f);
                uint16_t extra_field_len = fgetuint16(f);

                char zip_path[1024];

                /* these are unused.  */
                (void)lastmoddate;
                (void)lastmodtime;
                (void)compression_method;
                (void)version;

                f.read(zip_path, name_len);
                zip_path[name_len] = 0;

                auto extra_field = std::make_unique<char[]>(extra_field_len);
                f.read(extra_field.get(), extra_field_len);

                if (flags & 1)
                {
                    if (compr_size >= 12)
                    {
                        uint8_t* file = fileHead;
                        f.read((char*)file, 12);

                        if (flags & 8)
                        {
                            /* extended header format? */
                            file[12] = lastmodtime >> 8;
                            file[12 + 1] = lastmodtime;
                        }
                        else
                        {
                            file[12] = crc32 >> 24;
                            file[12 + 1] = crc32 >> 16;
                        }

                        LogInfo("found file '",
                            zip_path, "', (size cp/uc ",
                            Convert::ToString(compr_size), "/",
                            Convert::ToString(uncompr_size), ", flags ",
                            Convert::ToString(flags, 16), ", chk ",
                            Convert::ToString(file[12], 16),
                            Convert::ToString(file[12 + 1], 16), ")");
                        compr_size -= 12;
                    }
                    else
                    {
                        LogWarn("'", zip_path, "' is corrupted, skipping zipfile\n");
                        goto out;
                    }
                }
                else
                    LogInfo("'", zip_path, "' is not encrypted, skipping\n");

                auto compr = std::make_unique<char[]>(compr_size);
                f.read(compr.get(), compr_size);
            }
            else if (id == 0x08074b50UL)	/* extended local sig (?)  */
            {
                char local[12];
                f.read(local, 12);
            }
            else if (id == 0x30304b50UL)
            {
                /* ignore */
            }
            else if (id == 0x02014b50UL || id == 0x06054b50UL)
            {
                goto out;
            }
            else
            {
                LogWarn("found id ", Convert::ToString(id, 16), ", file is not a zipfile ver 2.xx, skipping");
                goto out;
            }
        }

    out:
        return maxUncomprSize;
    };

    std::atomic_bool done = false;

    const auto [fileSize, file] = [&]()
    {
        const auto raw = req(Op::fil);
        return std::make_tuple(Convert::FromString<uint64_t>(raw.substr(0, NumLen), 16), raw.substr(NumLen));
    }();
    const auto [passwordLen, alphabet] = [&]()
    {
        const auto raw = req(Op::tab);
        return std::make_tuple(Convert::FromString<uint64_t>(raw.substr(0, NumLen), 16), raw.substr(NumLen * 2));
    }();
    uint8_t fileHead[14]{ 0 };
    loadZip(file, fileHead);
	
    while (!done.load())
    {
        using DonType = std::nullopt_t;
        using WitType = uint64_t;
        using AccType = std::tuple<std::string, uint64_t>;
        using ReqResType = std::variant<DonType, WitType, AccType>;
        std::visit(ReqResVisitor{
            [&](const DonType&) { done.store(true); },
            [&](const WitType& x) { std::this_thread::sleep_for(std::chrono::seconds(x)); },
            [&](const AccType& x)
            {
                const auto& [base,offset] = x;
                static const auto reqDone = [&](const std::string& pw)
                {
                    done.store(true);
                    Socket::Client client(params.Address, params.Port);
                    client.Init();
                    client.Connect();
                    client.SendAll(String::StringCombineNew(ToString(Op::fid), pw));
                    client.Close();
                };
#ifdef CudaEnable
                if (params.Cuda)
                {
	                try
	                {
                        const auto flags = std::string(offset, '\x0');
                        FuckZipCrcCuda((uint8_t*)flags.data(), fileHead, base, alphabet, offset);
                        Enumerable::Range<std::uint64_t> range(offset);
                        std::for_each(std::execution::par_unseq, range.begin(), range.end(), [&](const uint64_t i)
                        {
                            if (static_cast<uint8_t>(flags.at(i)) != 0)
                            {
                                const auto pw = *PasswordAdd(base, i, alphabet);
                                const auto res = FuckZip<LLevel>(file, pw);
                                if (res) reqDone(pw);
                            }
                        });
	                }
	                catch (const std::exception& ex)
	                {
                        LogError("Client<", ToString(LLevel), ">::ReqResVisitor<const AccType& -> ()>: call cuda failed: ", ex.what());
	                }
                }
                else
#endif
                {
                    Enumerable::Range<uint64_t> range(offset);
                    std::for_each(std::execution::par, range.begin(), range.end(), [&](const uint64_t b)
                    {
                        const auto pw = PasswordAdd(base, b, alphabet).value();
                        const auto preRes = FuckZipCrc(fileHead, pw.data(), pw.length());
                        if (!preRes) return;
                        const auto res = FuckZip<LLevel>(file, pw);
                        if (res) reqDone(pw);
                    });
                }
                //LogInfo("crc: ", Convert::ToString(allLast.size()), "/", Convert::ToString(offset));
            }
            }, [&]()-> ReqResType
            {
                const auto cs = Convert::ToString(params.ChunkSize, 16);
                const std::string raw = reqData(Op::req, String::StringCombineNew(cs, std::string(NumLen - cs.length(), ' ')));
                switch (ToOp(raw.substr(0, OpLen)).value())
                {
                case Op::don:
                    return std::nullopt;
                case Op::wit:
                    return Convert::FromString<WitType>(raw.substr(OpLen, NumLen), 16);
                case Op::acc:
                    return AccType(raw.substr(OpLen, passwordLen), Convert::FromString<uint64_t>(raw.substr(OpLen + passwordLen, NumLen), 16));
                default:
                    LogError("unknow Op: ", raw.substr(0, OpLen));
                }
            }());
    }
}

struct ServerParams
{
    uint16_t Port;
    uint16_t ThreadNumber;
    const char* File;
    std::uint64_t FileSize;
    std::string Alphabet;
    std::string Begin;
};

template<LogLevel LLevel>
void Server(const ServerParams& params)
{
    Socket::Server server(params.Port);
    server.Init();
    server.Bind();
    server.Listen();
    const auto opFilStr = ToString(Op::fil);
    const auto fileSizeStr = Convert::ToString(params.FileSize, 16);
    const auto filHead = String::StringCombineNew(opFilStr, fileSizeStr, std::string(17 - fileSizeStr.length(), ' '));

    std::atomic_bool done = false;
    std::atomic_bool found = false;
    auto start = params.Begin;
    const auto n = params.Begin.length();
    const auto base = params.Alphabet.length();
    using Work = std::tuple<decltype(ServerParams::Begin), uint64_t>;
    Thread::Channel<Work> todo{};
    std::map<Work, decltype(std::chrono::system_clock::now())> works{};
    Thread::Synchronize worksRef([&]()->decltype(works)&
    {
        return works;
    });

    auto check = std::thread([&]()
    {
        while (!(done.load() && works.empty() && todo.Length() == 0))
        {
            using namespace std::chrono_literals;
            std::this_thread::sleep_for(+60s);
        }
    });

    const auto genNext = [&](const std::uint64_t size)->std::optional<Work>
    {
        if (found.load())
        {
            done.store(true);
            return std::nullopt;
        }

        if (todo.Length() != 0)
        {
            const auto [s, o] = todo.Read();
            if (o <= size) return Work(s, o);
            todo.Write(Work(s, o));
        }

        if (done.load()) return std::nullopt;

        const auto c = PasswordAdd(start, size, params.Alphabet);
        if (!c.has_value())
        {
            std::uint64_t offset = 0;
            std::string nc(n, '\0');
            const auto fin = std::string(n, params.Alphabet[base - 1]);
            while (nc != fin)
            {
                nc = PasswordAdd(start, offset++, params.Alphabet).value();
            }
            done.store(true);
            return Work(start, offset);
        }
        start = c.value();
        return std::optional(Work(*c, size));
    };

    Thread::Synchronize genNextSync(genNext);

    if constexpr (LLevel >= LogLevel::Error) LogLog("server start...");

    std::vector<std::thread> threadPool(params.ThreadNumber);

    std::generate(threadPool.begin(), threadPool.end(), [&]()
    {
        return std::thread([&]()
        {
            while (true)
            {
                try
                {
                    const auto client = server.Accept();
                    if (client.status == Socket::Server::ClientSucceeded)
                    {
                        const auto cli = client.value;
                        char reqBuf[BufLen + 1] = { 0 };
                        if (cli.Recv(reqBuf, BufLen))
                        {
                            char host[NI_MAXHOST] = { 0 };
                            getnameinfo((sockaddr*)(&cli.address), cli.addrLen,
                                host, NI_MAXHOST,
                                nullptr, 0,
                                NI_NUMERICHOST);
                            LogInfo(host, ":", Convert::ToString(ntohs(cli.address.sin_port)), ": ", reqBuf);
                            const auto op = ToOp(std::string(reqBuf).substr(0, 3)).value();
                            if (op == Op::fil)
                            {
                                const auto fl = Convert::ToString(params.FileSize, 16);
                                const auto dt = String::StringCombineNew(fl, std::string(NumLen - fl.length(), ' '), std::string(params.File, params.FileSize));
                                client.value.SendAll(dt);
                            }
                            else if (op == Op::tab)
                            {
                                const auto pl = Convert::ToString(params.Begin.length(), 16);
                                const auto al = Convert::ToString(params.Alphabet.length(), 16);
                                client.value.SendAll(String::StringCombineNew(
                                    pl, std::string(NumLen - pl.length(), ' '),
                                    al, std::string(NumLen - al.length(), ' '),
                                    params.Alphabet));
                            }
                            else if (op == Op::req)
                            {
                                const auto size = Convert::FromString<std::uint64_t>(std::string_view(reqBuf + 3, NumLen), 16);
                                LogDebug("size: ", Convert::ToString(size));
                                const auto wk = genNextSync(size);
                                if (wk.has_value())
                                {
                                    const auto [s, o] = *wk;
                                    const auto oStr = Convert::ToString(o, 16);
                                    const auto resp = String::StringCombineNew(ToString(Op::acc), s, oStr, std::string(NumLen - oStr.length(), ' '));
                                    LogDebug("resp: ", resp);
                                    cli.Send(resp.data(), resp.length());
                                    worksRef().emplace(wk.value(), std::chrono::system_clock::now());
                                }
                                else if (found.load())
                                {
                                    const auto resp = ToString(Op::don);
                                    cli.SendAll(resp);
                                    LogDebug("resp: ", resp);
                                }
                                else
                                {
                                    const auto resp = String::StringCombineNew(ToString(Op::wit), Convert::ToString(30, 16));
                                    cli.SendAll(resp);
                                    LogDebug("resp: ", resp);
                                }
                            }
                            else if (op == Op::wrk)
                            {
                                std::string s(n, '\0');
                                cli.Recv(s.data(), n);
                                char oBuf[NumLen]{ 0 };
                                cli.Recv(oBuf, NumLen);
                                const auto o = Convert::FromString<uint64_t>(std::string_view(oBuf, NumLen), 16);
                                worksRef()[Work(s, o)] = std::chrono::system_clock::now();
                            }
                            else if (op == Op::fid)
                            {
                                std::string pw = cli.RecvAll();
                                LogLog("password found! ", "\"", pw, "\"");
                                found.store(true);
                            }
                        }
                    }
                    client.value.Close();
                }
                catch (const std::exception& ex)
                {
                    if constexpr (LLevel >= LogLevel::Error) LogError(MacroFunctionName, ": ", ex.what());
                }
            }
        });
    });
    for (auto& thread : threadPool) { thread.join(); }
    check.join();
}

#define LogLevelMapper(logLevel, func, ...)\
{\
    if (logLevel == LogLevel::None)  func<LogLevel::None> (__VA_ARGS__);\
    if (logLevel == LogLevel::Error) func<LogLevel::Error>(__VA_ARGS__);\
    if (logLevel == LogLevel::Warn)  func<LogLevel::Warn> (__VA_ARGS__);\
    if (logLevel == LogLevel::Log)   func<LogLevel::Log>  (__VA_ARGS__);\
    if (logLevel == LogLevel::Info)  func<LogLevel::Info> (__VA_ARGS__);\
    if (logLevel == LogLevel::Debug) func<LogLevel::Debug>(__VA_ARGS__);\
}

int main(int argc, char* argv[])
{
    using ArgumentsParse::Argument;
#define ArgumentsFunc(arg) [&](decltype(arg)::ConvertFuncParamType value) -> decltype(arg)::ConvertResult
#define ArgumentsFuncConvert(arg) ArgumentsFunc(arg) { return { Convert::FromString<decltype(arg)::ValueType>(value), {} }; }
#define ArgumentsFuncValue(arg, val) ArgumentsFunc(arg) { return { val, {} }; }

    ArgumentsParse::Arguments args{};

    Argument<bool, 0> helpArg
    {
        "--help",
        "help info",
        false,
        ArgumentsFunc(helpArg)
        {
            return { true, {} };
        }
    };
    Argument<bool, 0> serverArg
    {
        "--server",
        "run as server",
        false,
        ArgumentsFuncValue(serverArg, true)
    };
    Argument<bool, 0> cudaArg
    {
        "--cuda",
        "enable cuda",
        false,
        ArgumentsFuncValue(cudaArg, true)
    };
    Argument<std::filesystem::path> fileArg
    {
        "-f",
        "file path"
    };
    Argument<decltype(ServerParams::Alphabet)> alphabetArg
    {
        "-a",
        "alphabet",
        ArgumentsFunc(alphabetArg)
        {
            std::string val(value);
            const auto it = std::unique(val.begin(), val.end());
            val.erase(it, val.end());
            std::sort(val.begin(), val.end());
            return { val, {} };
        }
    };
    //Argument<std::filesystem::path> passwordTablePathArg
    //{
    //	"-t",
    //	"password table file path"
    //};
    Argument<decltype(ClientParams::ChunkSize)> chunkSizeArg
    {
        "-c",
        "chunk size",
        ArgumentsFuncConvert(chunkSizeArg)
    };
    Argument<decltype(ClientParams::Address)> addressArg
    {
        "-i",
        "ip address"
    };
    Argument<decltype(ServerParams::Port)> portArg
    {
        "-p",
        "listen port (50000)",
        50000,
        ArgumentsFuncConvert(portArg)
    };
    Argument<decltype(ServerParams::ThreadNumber)> threadNumberArg
    {
        "-t",
        "Thread Number (100)",
        100,
        ArgumentsFuncConvert(threadNumberArg)
    };
    Argument<uint64_t> passwordLengthArg
    {
        "-l",
        "password length",
        ArgumentsFuncConvert(passwordLengthArg)
    };
    Argument passwordBeginArg
    {
        "-b",
        "password begin"
    };
    Argument<LogLevel> logLevelArg
    {
        "-ll",
        "log level " + LogLevelDesc(ToString(LogLevel::Log)),
        LogLevel::Log,
        ArgumentsFunc(logLevelArg)
        {
            return { ToLogLevel(std::string(value)), {} };
        }
    };
    Argument<std::filesystem::path> logFileArg
    {
        "-lf",
        "log file path"
    };
    Argument<bool, 0> consoleArg
    {
        "-ldc",
        "disable console",
        true,
        ArgumentsFuncValue(consoleArg, false)
    };

    args.Add(helpArg);
    args.Add(serverArg);
    args.Add(cudaArg);
    args.Add(fileArg);
    args.Add(alphabetArg);
    //args.Add(passwordTablePathArg);
    args.Add(addressArg);
    args.Add(chunkSizeArg);
    args.Add(portArg);
    args.Add(threadNumberArg);
    args.Add(passwordLengthArg);
    args.Add(passwordBeginArg);
    args.Add(logLevelArg);
    args.Add(logFileArg);
    args.Add(consoleArg);

    const auto helpPrinter = [&]()
    {
        Console::WriteLine("usage: ", argv[0], " [options]");
        Console::WriteLine(args.GetDesc());
        Console::WriteLine(R"(
Usage:
    As Server:
        --server -a -f -l [-p 50000] [-t 100] [-b]
    As Client:
        -c -i [--cuda]
    [Log]:
    	[-ll Log] [-lf] [-ldc]

Example:
    Server:
        --server -a 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz -f Z:\test.zip -l 5 -ll Debug
    Client:
        -c 1000000 -i 127.0.0.1 --cuda
)");
    };
    try
    {
        std::thread log;
        args.Parse(argc, argv);

        if (args.Value(helpArg))
        {
            helpPrinter();
            exit(EXIT_SUCCESS);
        }

        const auto logLevel = args.Value(logLevelArg);

        LogInfo("\n", args.GetValuesDesc({
                args.GetValuesDescConverter<std::string          >([](const auto& x) { return x; }),
                args.GetValuesDescConverter<bool                 >([](const auto& x) { return x ? "true" : "false"; }),
                args.GetValuesDescConverter<std::filesystem::path>([](const auto& x) { return x.string(); }),
                args.GetValuesDescConverter<std::uint64_t        >([](const auto& x) { return Convert::ToString(x); }),
                args.GetValuesDescConverter<std::uint16_t        >([](const auto& x) { return Convert::ToString(x); }),
                args.GetValuesDescConverter<LogLevel             >([](const auto& x) { return ToString(x); }),
            }));
        log = std::thread([](const LogLevel level, const std::optional<std::filesystem::path>& file, const bool console)
        {
            std::ofstream fs{};
            if (file.has_value()) fs.open(*file, std::ios::app);
            const auto toString = [](const LoggerType::Time& time)
            {
                const auto timeT = std::chrono::system_clock::to_time_t(time);
                tm local{};
                Time::Local(&local, &timeT);
                return String::FromStreamNew(std::put_time(&local, "%F %X"));
            };
            while (true)
            {
                const auto [ll, msg] = Log.Chan.Read();
                if (ll > level) continue;
                const auto& [tp, id, f, line, dt] = msg;
                const auto out = String::StringCombineNew("[", toString(tp), "][", ToString(ll), "][0x", String::FromStreamNew(id, std::hex), "][", std::filesystem::path(f).filename().string(), ":", line, "] ", dt);
                if (console)
                {
                    Console::WriteLine(out);
                }
                if (file.has_value())
                {
                    fs.write(out.data(), out.length());
                    fs.flush();
                }
                if (ll == LogLevel::None)
                {
                    break;
                }
            }
        }, logLevel, args.Get(logFileArg), args.Value(consoleArg));

        if (args.Value(serverArg))
        {
            const auto file = args.Value(fileArg);
            const auto alphabet = args.Value(alphabetArg);
            const auto data = File::ReadAllText(file);
            ServerParams params
            {
                args.Value(portArg),
                args.Value(threadNumberArg),
                data.data(),
                file_size(file),
                alphabet,
                std::string(args.Value(passwordLengthArg), alphabet[0])
            };
            LogLevelMapper(logLevel, Server, params)
        }
        else
        {
            ClientParams params
            {
                args.Value(addressArg),
                args.Value(portArg),
                args.Value(chunkSizeArg),
                args.Value(cudaArg)
            };
            LogLevelMapper(logLevel, Client, params)
        }
        LogImpl(LogLevel::None, "{ok}.");
        log.join();
    }
    catch (const std::exception& ex)
    {
        Console::WriteLine(ex.what());
        Console::WriteLine();
        helpPrinter();
        exit(EXIT_FAILURE);
    }
}
