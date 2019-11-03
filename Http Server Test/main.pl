use Socket;
my $port = 18852;
my $protocol = getprotobyname("tcp");
socket(SOCK, PF_INET, SOCK_STREAM, $protocol);
setsockopt(SOCK, SOL_SOCKET, SO_REUSEADDR, 1);
bind(SOCK, sockaddr_in($port, INADDR_ANY));
listen(SOCK, SOMAXCONN);
while(accept(CLIENT, SOCK))
{
    print CLIENT "HTTP/1.1 200 OK\r\nContent-length:56\r\nContent-Type: text/html; charset=UTF-8\r\n\r\n<!DOCTYPE html><html><body>Goodbye, world!</body></html>";
    close CLIENT;
}
