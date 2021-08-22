#include <iostream>
#include <asio.hpp>

using asio::ip::tcp;

int main(int argc, char* argv[])
{
    try
    {
        asio::io_context io_context;

        tcp::resolver resolver(io_context);
        tcp::resolver::results_type endpoints =
            resolver.resolve("127.0.0.1", "80");

        tcp::socket socket(io_context);
        asio::connect(socket, endpoints);
    	asio::error_code error;
        std::string dt = "GET / HTTP\r\n"
"\r\n"
"";
        socket.write_some(asio::buffer(dt), error);
    	
        for (;;)
        {
            std::string buf(4096, '\x0');

            size_t len = socket.read_some(asio::buffer(buf), error);

            if (error == asio::error::eof)
                break;
            if (error)
	            throw asio::system_error(error);

            std::cout.write(buf.data(), len);
        }
    }
    catch (std::exception& e)
    {
        std::cerr << e.what() << std::endl;
    }

    return 0;
}
