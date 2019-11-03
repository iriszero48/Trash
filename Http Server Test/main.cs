using System;
using System.Net;
using System.Net.Sockets;
using System.Text;

namespace Temp
{
    class TEMP
    {
        static void Main()
        {
            while (true)
            {
                try
                {
                    const string msg = "HTTP/1.1 200 OK\r\nContent-length:56\r\nContent-Type: text/html; charset=UTF-8\r\n\r\n<!DOCTYPE html><html><body>Goodbye, world!</body></html>";
                    TcpListener tcpListener = new TcpListener(IPAddress.Any, 8850);
                    tcpListener.Start();
                    while (true)
                    {
                        Socket socketConnection = tcpListener.AcceptSocket();
                        socketConnection.Send(Encoding.ASCII.GetBytes(msg.ToCharArray(), 0, msg.Length));
                        socketConnection.Disconnect(true);
                    }
                }
                catch()
                {
                    // ignored
                }
            }
        }
    }
}
