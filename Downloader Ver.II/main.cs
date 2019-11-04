using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System;
using System.Threading.Tasks;
using static System.Console;

namespace Downloader_Ver.II
{
    class Program
    {
        private static string uRL, savePath, username, password, speed;

        public static string URL { get => uRL; set => uRL = value; }
        public static string SavePath { get => savePath; set => savePath = value; }
        public static string Username { get => username; set => username = value; }
        public static string Password { get => password; set => password = value; }
        public static string Speed { get => speed; set => speed = value; }

        static void Main(string[] args)
        {
            Write("URL :");
            URL = ReadLine();
            Write("SavePath :");
            SavePath = ReadLine();
            Write("Username :");
            Username = ReadLine();
            Write("Password :");
            Password = ReadLine();
            Write("Speed :");
            Speed = ReadLine();
            Bulid();
            ProcessStartInfo processInfo = new ProcessStartInfo($"{SavePath}\\DownloadVer.II.bat", "2>&1")
            {
                WorkingDirectory = Directory.GetCurrentDirectory(),
                CreateNoWindow = true,
                UseShellExecute = false,
                RedirectStandardOutput = true
            };
            Process proc = Process.Start(processInfo);
            proc.EnableRaisingEvents = true;
            proc.Exited += new EventHandler(ProcExit);
            proc.OutputDataReceived += new DataReceivedEventHandler(OutputHandler);
            proc.BeginOutputReadLine();
            proc.WaitForExit();
        }
        private static void ProcExit(object sender, EventArgs e)
        {
            WriteLine("Done.");
        }
        private static void OutputHandler(object sender, DataReceivedEventArgs e)
        {
            if (!String.IsNullOrEmpty(e.Data))
            {
                WriteLine(e.Data);
            }
        }
        private static void Bulid()
        {
            File.Copy("wget.exe", $"{SavePath}\\wget.exe", true);
            FileStream fileStream = new FileStream($"{SavePath}\\DownloadVer.II.bat", FileMode.Create, FileAccess.Write);
            StreamWriter streamWriter = new StreamWriter(fileStream);
            streamWriter.Write($"wget.exe -c -e ");
            bool hasHttps = URL[4] == 's';
            bool hasUsername = Username != "";
            bool hasPassword = Password != "";
            bool hasSpeed = Speed != "";
            if (hasHttps)
            {
                if (hasUsername)
                {
                    if (hasSpeed)
                    {
                        streamWriter.Write($"--limit-rate={Speed} robots=off -x --user={Username} --password={Password} -m --no-check-certificate ");
                    }
                    else
                    {
                        streamWriter.Write($"robots=off -x --user={Username} --password={Password} -m --no-check-certificate ");
                    }
                }
                else
                {
                    if (hasSpeed)
                    {
                        streamWriter.Write($"--limit-rate={Speed} robots=off -x -m --no-check-certificate ");
                    }
                    else
                    {
                        streamWriter.Write($"robots=off -x -m --no-check-certificate ");
                    }
                }
            }
            else
            {
                if (hasUsername)
                {
                    if (hasSpeed)
                    {
                        streamWriter.Write($"--limit-rate={Speed} robots=off -x --user={Username} --password={Password} -m ");
                    }
                    else
                    {
                        streamWriter.Write($"robots=off -x --user={Username} --password={Password} -m ");
                    }
                }
                else
                {
                    if (hasSpeed)
                    {
                        streamWriter.Write($"--limit-rate={Speed} robots=off -x -m ");
                    }
                    else
                    {
                        streamWriter.Write($"robots=off -x -m ");
                    }
                }
            }
            streamWriter.Write($"\"{URL}\"");
            streamWriter.Close();
        }
    }
}
