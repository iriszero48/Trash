using System;
using System.IO;
using System.Net;
using System.Text;

namespace WebGet
{
    /*
    class GW
    {
        public void get()
        {
            while (true)
            {
                var get = SendRequest("http://ctf.h-k.pw/web2/", Encoding.UTF8);               
                if (get != "")
                {
                    Console.Write(get);
                }
            }
        }
        public String SendRequest(String url, Encoding encoding)
        {
            HttpWebRequest webRequest = (HttpWebRequest)WebRequest.Create(url);
            webRequest.Method = "GET";
            HttpWebResponse webResponse = (HttpWebResponse)webRequest.GetResponse();
            StreamReader sr = new StreamReader(webResponse.GetResponseStream(), encoding);
            return sr.ReadToEnd();
        }
    }*/
    class Program
    {
       
        static void Main(string[] args)
        {
            Console.WriteLine(DateTime.Now.ToString("F"));
            /*List<GW> gw = new List<GW>();
                List<Thread> th = new List<Thread>();
                for(int i=0;i<1;++i)
                {
                    gw.Add(new GW());
                    th.Add(new Thread(gw[i].get));
                    Console.WriteLine($"create thread {i + 1}");
                }

                // Create the thread object. This does not start the thread.
                //GW workerObject = new GW();
                //Thread workerThread = new Thread(workerObject.get);

                // Start the worker thread.
                //workerThread.Start();
                for(int i=0;i<th.Count;++i)
                {
                    th[i].Start();
                    Console.WriteLine($"start thread {i + 1}");
                }
                Console.WriteLine("main thread: Starting worker thread...");

                // Loop until worker thread activates.
                //while (!workerThread.IsAlive) ;
                bool cou = true;
                while(cou)
                {
                    for(int i=0;i<th.Count;++i)
                    {
                        if(!th[i].IsAlive)
                        {
                            cou = false;
                            break;
                        }
                    }
                }

                // Put the main thread to sleep for 1 millisecond to
                // allow the worker thread to do some work:
                Thread.Sleep(1);

                // Request that the worker thread stop itself:
                

                // Use the Join method to block the current thread 
                // until the object's thread terminates.
                for(int i=0;i<th.Count;++i)
                {
                    th[i].Join();
                    Console.WriteLine($"stop thread {i + 1}");
                }
                // workerThread.Join();
                Console.WriteLine("main thread: Worker thread has terminated.");
            Console.WriteLine(DateTime.Now.ToString("F"));*/
            while (true)
            {
                var get = SendRequest("http://ctf.h-k.pw/web2/", Encoding.UTF8);
                Console.Write(get);
                if (get != "")
                {
                    break;
                }
            }
            Console.WriteLine("\n"+DateTime.Now.ToString("F"));
        }
        public static String SendRequest(String url, Encoding encoding)
        {
            HttpWebRequest webRequest = (HttpWebRequest)WebRequest.Create(url);
            webRequest.Method = "GET";
            HttpWebResponse webResponse = (HttpWebResponse)webRequest.GetResponse();
            StreamReader sr = new StreamReader(webResponse.GetResponseStream(), encoding);
            return sr.ReadToEnd();
        }
    }
}
