using System;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Threading;
using static System.Console;

namespace Transponder
{
    internal static class Program
    {
        private static long SendCounter;
        private static long RecvCounter;
        private static void Main(string[] args)
        {
            new Thread(() =>
            {
                while (true)
                {
                    var output =
                        $"Recv:{Math.Round((double) (RecvCounter / 1024))}kB - Send:{Math.Round((double) (SendCounter / 1024))}kB";
                    Write($"{new string('\u0008', output.Length)}{output}");
                    Thread.Sleep(1000);
                }
            }).Start();
            var clientTcpListener = new TcpListener(IPAddress.Any,8848);
            var serverTcpListener = new TcpListener(IPAddress.Any,8849);
            clientTcpListener.Start();
            serverTcpListener.Start();
            while (true)
            {
                try
                {
                    WriteLine("Start...");
                    var tc1 = clientTcpListener.AcceptTcpClient();
                    var tc2 = serverTcpListener.AcceptTcpClient();
                    tc1.SendTimeout = 30000;
                    tc1.ReceiveTimeout = 30000;
                    tc2.SendTimeout = 30000;
                    tc2.ReceiveTimeout = 30000;
                    //ThreadPool.QueueUserWorkItem(Transfer, new[] { tc1, tc2 });
                    //ThreadPool.QueueUserWorkItem(Transfer, new[] { tc2, tc1 });
                    var t1 = new Thread(() => { Transfer(new object[] { tc1, tc2, false }); });
                    var t2 = new Thread(() => { Transfer(new object[] { tc2, tc1, true }); });
                    t1.Start();
                    t2.Start();
                    t1.Join();
                    t2.Join();
                }
                catch (Exception e)
                {
                    WriteLine(e.Message);
                }
            }
        }

        private static void Transfer(object obj)
        {
            try
            {
                var bt = new byte[1024];
                var argv = obj as object[];
                var tc1 = argv[0] as TcpClient;
                var tc2 = argv[1] as TcpClient;
                var isRecv = (bool) argv[2];
                var ns1 = tc1.GetStream();
                var ns2 = tc2.GetStream();
                try
                {
                    int len;
                    while ((len = ns1.Read(bt, 0, bt.Length)) > 0)
                    {
                        ns2.Write(bt, 0, len);
                        if (isRecv) RecvCounter += len;
                        else SendCounter += len;
                    }
                }
                catch (Exception e)
                {
                    ns1.Dispose();
                    ns2.Dispose();
                    tc1.Close();
                    tc2.Close();
                    WriteLine(e.Message);
                }
            }
            catch (Exception e)
            {
                WriteLine(e);
            }
        }
    }
}
