<?php
$socket = socket_create(AF_INET, SOCK_STREAM, 0);
socket_bind($socket, 0, 18853);
socket_listen($socket);
while (true) if ($client = @socket_accept($socket))
{
    socket_write($client, "HTTP/1.1 200 OK\r\nContent-length:56\r\nContent-Type: text/html; charset=UTF-8\r\n\r\n<!DOCTYPE html><html><body>Goodbye, world!</body></html>");
    socket_close($client);
}
