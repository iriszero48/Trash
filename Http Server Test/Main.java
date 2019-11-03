import java.io.IOException;
import java.io.PrintWriter;
import java.net.ServerSocket;
import java.net.Socket;

public class Main
{
    public static void main(String[] args) throws IOException
    {
        ServerSocket listener = new ServerSocket(18849);
        while(true)
        {
            Socket sock = listener.accept();
            new PrintWriter(sock.getOutputStream(), true).println("HTTP/1.1 200 OK\r\nContent-length:56\r\nContent-Type: text/html; charset=UTF-8\r\n\r\n<!DOCTYPE html><html><body>Goodbye, world!</body></html>");
            sock.close();
        }
    }
}
