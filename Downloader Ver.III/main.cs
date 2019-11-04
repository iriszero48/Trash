using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Threading;
using UnityEngine;
using UnityEngine.Experimental.UIElements;
using UnityEngine.UI;

public class UGUI : MonoBehaviour
{
    public string URL;
    public string SavePath;
    public string Result;

    Thread c;
    private static string Username, Password, Speed;

    // Use this for initialization
    void Start()
    {
        URL = "URL";
        SavePath = "SavePath";
        Result = "Downloader Ver.III";
        Username = "";
        Password = "";
        Speed = "";
        
    }
    // Update is called once per frame
    void Update()
    {

    }
    private void OnGUI()
    {
        URL = GUI.TextField(new Rect(Screen.width / 2 - (300 / 2), Screen.height / 2 - 100, 300, 30), URL);
        SavePath = GUI.TextField(new Rect(Screen.width / 2 - (300 / 2), Screen.height / 2 - 50, 300, 30), SavePath);
        GUI.Label(new Rect(Screen.width / 2 - (600 / 2), Screen.height / 2 + 100, 600, 200), Result);
        if (GUI.Button(new Rect(Screen.width / 2 - (300 / 2), Screen.height / 2, 300, 30), "Download"))
        {
            Result = "Build";
            Bulid();            
            Result = "Start";
            Cmd();
        }
    }
    private void Bulid()
    {
        File.Copy("Z:\\wget.exe", SavePath + "\\wget.exe", true);
        FileStream fileStream = new FileStream(SavePath + "\\DownloadVer.III.bat", FileMode.Create, FileAccess.Write);
        StreamWriter streamWriter = new StreamWriter(fileStream);
        streamWriter.Write("Z:\\wget.exe -c -e ");
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
                    streamWriter.Write("--limit-rate=" + Speed + " robots=off -x --user=" + Username + " --password=" + Password + " -m --no-check-certificate ");
                }
                else
                {
                    streamWriter.Write("robots=off -x --user=" + Username + " --password=" + Password + " -m --no-check-certificate ");
                }
            }
            else
            {
                if (hasSpeed)
                {
                    streamWriter.Write("--limit-rate=" + Speed + " robots=off -x -m --no-check-certificate ");
                }
                else
                {
                    streamWriter.Write("robots=off -x -m --no-check-certificate ");
                }
            }
        }
        else
        {
            if (hasUsername)
            {
                if (hasSpeed)
                {
                    streamWriter.Write("--limit-rate=" + Speed + " robots=off -x --user=" + Username + " --password=" + Password + " -m ");
                }
                else
                {
                    streamWriter.Write("robots=off -x --user=" + Username + " --password=" + Password + " -m ");
                }
            }
            else
            {
                if (hasSpeed)
                {
                    streamWriter.Write("--limit-rate=" + Speed + " robots=off -x -m ");
                }
                else
                {
                    streamWriter.Write("robots=off -x -m ");
                }
            }
        }
        streamWriter.Write("\"" + URL + "\"");
        streamWriter.Close();
    }
    private void Cmd()
    {
        c = new Thread(() =>
        {
            ProcessStartInfo processInfo = new ProcessStartInfo(SavePath + "\\DownloadVer.III.bat", "2>&1")
            {
                WorkingDirectory = Directory.GetCurrentDirectory(),
                CreateNoWindow = false,
                UseShellExecute = false,
                RedirectStandardOutput = true
            };
            Process proc = Process.Start(processInfo);
            proc.EnableRaisingEvents = true;
            proc.Exited += new EventHandler(ProcExit);
            proc.OutputDataReceived += new DataReceivedEventHandler(OutputHandler);
            proc.BeginOutputReadLine();
            proc.WaitForExit();
        });
        c.Start();
    }

    private void ProcExit(object sender, EventArgs e)
    {
        Result = "Done.";
    }
    private void OutputHandler(object sender, DataReceivedEventArgs e)
    {
        if (!String.IsNullOrEmpty(e.Data))
        {
            Result = e.Data;
        }
    }
}
