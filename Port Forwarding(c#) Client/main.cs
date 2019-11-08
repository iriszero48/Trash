using System;
using System.Diagnostics;
using System.Net.Sockets;
using System.Threading;
using static System.Console;

namespace Client
{
    internal static class Program
    {
        private static void Main(string[] args)
        {
            var pool = new Thread[1];
            for (int i = 0; i < pool.Length; i++)
            {
                pool[i] = new Thread(() =>
                {

                    while (true)
                    {
                        try
                        {
                            WriteLine("Link Start...");
                            //var tc1 = new TcpClient("47.104.88.12", 8849);
                            var tc1 = new TcpClient("183.60.111.81", 8849);
                            var tc2 = new TcpClient("localhost", 27015);
                            tc1.SendTimeout = 30000;
                            tc1.ReceiveTimeout = 30000;
                            tc2.SendTimeout = 30000;
                            tc2.ReceiveTimeout = 30000;
                            //new Thread(() => { Transfer(new[] { tc1, tc2 }); }).Start();
                            //new Thread(() => { Transfer(new[] { tc2, tc1 }); }).Start();
                            //ThreadPool.QueueUserWorkItem(Transfer, new[] { tc1, tc2 });
                            //ThreadPool.QueueUserWorkItem(Transfer, new[] { tc2, tc1 });

                            var t1 = new Thread(() => { Transfer(new[] {tc1, tc2}); }); 
                            //var t2 = new Thread(() => { Transfer(new[] {tc2, tc1}); });
                            t1.Start();
                            //t2.Start();
                            t1.Join();
                            //t2.Join();
                            //Thread.Sleep(1000);
                        }
                        catch (Exception e)
                        {
                            WriteLine(e.Message);
                        }
                    }
                });
                pool[i].Start();
            }

            for (int i = 0; i < pool.Length; i++)
            {
                pool[i].Join();
            }
        }

        private static void Transfer(object obj)
        {
            var bt = new byte[1024];
            var tc1 = ((TcpClient[])obj)[0];
            var tc2 = ((TcpClient[])obj)[1];
            var ns1 = tc1.GetStream();
            var ns2 = tc2.GetStream();
            try
            {
                int len;
                while ((len = ns1.Read(bt, 0, bt.Length)) == 1024)
                {
                    ns2.Write(bt, 0, len);
                    Write("+");
                }
                ns2.Write(bt, 0, len);

                while ((len = ns2.Read(bt, 0, bt.Length)) == 1024)
                {
                    ns1.Write(bt, 0, len);
                    Write("+");
                }
                ns1.Write(bt, 0, len);

            }
            catch (Exception e)
            {
                ns1.Dispose();
                ns2.Dispose();
                tc1.Close();
                tc2.Close();
                WriteLine(e.Message);
            }
            WriteLine("-");
        }
    }
}
