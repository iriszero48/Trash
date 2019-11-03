#!/usr/bin/gawk -f
BEGIN {
    HttpService = "/inet/tcp/8848/0/0"
    while(1){
        print "HTTP/1.1 200 OK\r\nContent-length:56\r\nContent-Type: text/html; charset=UTF-8\r\n\r\n<!DOCTYPE html><html><body>Goodbye, world!</body></html>" |& HttpService
        while((HttpService |& getline) > 0) 1
        close(HttpService)
    }
}
