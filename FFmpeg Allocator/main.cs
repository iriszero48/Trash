using System;
using System.IO;
using System.Diagnostics;
using System.Collections.Generic;
using System.Threading;

namespace FFmpegAllocator
{
    class Program
    {
        static void Main(string[] args)
        {
            if (args.Length == 2)
            {
                var donePath = Path.Combine(args[1], "done");
                var rawPath = Path.Combine(args[1], "raw");
                if (!Directory.Exists(donePath)) Directory.CreateDirectory(donePath);
                if (!Directory.Exists(rawPath)) Directory.CreateDirectory(rawPath);
                foreach (var x in Directory.EnumerateFiles(args[1]))
                {
                    var filename = Path.GetFileName(x);
                    var newFilename = Path.Combine(donePath, filename);
                    var cmd = File.ReadAllText(args[0]).Replace("$$$input$$$", x).Replace("$$$output$$$", newFilename);
                    Console.WriteLine($"[*] {x} => {newFilename}\n\tffmpeg {cmd}");
                    var ffmpeg = new Process
                    {
                        StartInfo = new ProcessStartInfo("ffmpeg.exe", cmd)
                        {
                            UseShellExecute = true,
                            CreateNoWindow = false
                        }
                    };
                    ffmpeg.Start();
                    ffmpeg.WaitForExit();
                    ffmpeg.Close();
                    File.Move(x, Path.Combine(rawPath, filename));
                }
            }
            if (args.Length == 3)
            {
                var donePath = Path.Combine(args[2], "done");
                var rawPath = Path.Combine(args[2], "raw");
                if (!Directory.Exists(donePath)) Directory.CreateDirectory(donePath);
                if (!Directory.Exists(rawPath)) Directory.CreateDirectory(rawPath);
                var count = int.Parse(args[1]);
                var threads = new List<Thread>();
                var lck = new Semaphore(count, count);
                foreach (var x in Directory.EnumerateFiles(args[2]))
                {
                    threads.Add(new Thread(() => 
                    {
                        lck.WaitOne();
                        var newFilename = Path.Combine(donePath, Path.GetFileName(x));
                        var cmd = File.ReadAllText(args[0]).Replace("$$$input$$$", x).Replace("$$$output$$$", newFilename);
                        Console.WriteLine($"[*] {x} => {newFilename}\n\tffmpeg {cmd}");
                        var ffmpeg = new Process
                        {
                            StartInfo = new ProcessStartInfo("ffmpeg.exe", cmd)
                            {
                                UseShellExecute = true,
                                CreateNoWindow = false
                            }
                        };
                        ffmpeg.Start();
                        ffmpeg.WaitForExit();
                        ffmpeg.Close();
                        File.Move(x, Path.Combine(rawPath, Path.GetFileName(x)));
                        lck.Release();
                    }));
                }
                foreach (var t in threads) t.Start();
                foreach (var t in threads) t.Join();
            }
            else
            {
                Console.Error.WriteLine("ConfigPath (Semaphore) DirectoryPath");
                Environment.Exit(1);
            }
        }
    }
}
