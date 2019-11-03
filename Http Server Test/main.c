#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <err.h>

char response[] = "HTTP/1.1 200 OK\r\nContent-length:56\r\nContent-Type: text/html; charset=UTF-8\r\n\r\n<!DOCTYPE html><html><body>Goodbye, world!</body></html>";

int main()
{
    int one = 1, client_fd;
    struct sockaddr_in svr_addr, cli_addr;
    socklen_t sin_len = sizeof(cli_addr);
    int sock = socket(AF_INET, SOCK_STREAM, 0);
    setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(int));
    int port = 8849;
    svr_addr.sin_family = AF_INET;
    svr_addr.sin_addr.s_addr = INADDR_ANY;
    svr_addr.sin_port = htons(port);
    bind(sock, (struct sockaddr *) &svr_addr, sizeof(svr_addr));
    listen(sock, 5);
    while (1)
    {
        client_fd = accept(sock, (struct sockaddr *) &cli_addr, &sin_len);
        write(client_fd, response, sizeof(response) - 1);
        close(client_fd);
    }
}
