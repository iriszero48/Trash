using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using Microsoft.WindowsAPICodePack.Dialogs;
using System.IO;
using System.Diagnostics;
using System.Windows.Threading;
using System.Threading;

namespace Downloader_Ver.I
{
    /// <summary>
    /// MainWindow.xaml 的交互逻辑
    /// </summary>
    public partial class MainWindow : Window
    {
        public Thread cmd;
        public MainWindow()
        {
            InitializeComponent();
            Username.Text = "";
            Password.Text = "";
            URL.Text = "";
            SavePathTextBox.Text = "";
            Speed.Text = "";
        }
        private void URLPath(object sender, RoutedEventArgs e)
        {

        }
        private void SavePath(object sender, RoutedEventArgs e)
        {
            var dialog = new CommonOpenFileDialog
            {
                IsFolderPicker = true
            };
            if (dialog.ShowDialog() == CommonFileDialogResult.Ok)
            {
                SavePathTextBox.Text = dialog.FileName;
            }
        }
        private void GetUsername(object sender, RoutedEventArgs e)
        {

        }
        private void GetPassword(object sender, RoutedEventArgs e)
        {

        }
        private void Download(object sender, RoutedEventArgs e)
        {
            DownloadButton.Content = "Stop";
            BuildWget();
            BulidBat();
            StartCmd();
        }
        public void ProcessOuputHandler(object sendingProcess, DataReceivedEventArgs outLine)
        {
            if (!String.IsNullOrEmpty(outLine.Data))
            {
                if (!Result.Dispatcher.CheckAccess())
                {
                    // Called from a none ui thread, so use dispatcher
                    ShowLoggingDelegate showLoggingDelegate = new ShowLoggingDelegate(ShowLogging);
                    Result.Dispatcher.Invoke(DispatcherPriority.Normal, showLoggingDelegate, outLine.Data);
                }
                else
                {
                    // Called from UI trhead so just update the textbox
                    ShowLogging(outLine.Data);
                    Thread.Sleep(100);
                };
            }
        }
        private delegate void ShowLoggingDelegate(string text);
        private delegate void ChangeButtonDelegate();
        private static Action EmptyDelegate = delegate () { };
        private static Action NullDelegate = delegate () { };
        /// <summary>
        /// Show the logging on screen
        /// </summary>
        /// <param name="text"></param>
        private void ShowLogging(string text)
        {
            Result.AppendText(text+Environment.NewLine);
            Result.ScrollToEnd();
        }
        private void ChangeStatic()
        {
            DownloadButton.Content = "Download";
            File.Delete($"{SavePathTextBox.Text}\\DownloadVer.I.bat");
            File.Delete($"{SavePathTextBox.Text}\\wget.exe");
        }
        private void BuildWget()
        {
            File.WriteAllBytes($"{SavePathTextBox.Text}\\wget.exe", StringToHex(WgetHex()));
        }
        public static byte[] StringToHex(string hexStr)
        {
            int i;
            int len;
            byte[] hexByte;

            len = hexStr.Length / 2;
            len = (hexStr.Length % 2 == 1) ? len + 1 : len;
            hexByte = new byte[len];

            for (i = 0; i < len; i++)
            {
                if ((i == len - 1) && (hexStr.Length % 2 == 1))
                {
                    hexByte[i] = Convert.ToByte(hexStr.Substring(i * 2, 1), 16);
                }
                else
                {
                    hexByte[i] = Convert.ToByte(hexStr.Substring(i * 2, 2), 16);
                }
            }
            return hexByte;
        }
        private void BulidBat()
        {
            //File.Copy("wget.exe", $"{SavePathTextBox.Text}\\wget.exe", true);
            FileStream fileStream = new FileStream($"{SavePathTextBox.Text}\\DownloadVer.I.bat", FileMode.Create, FileAccess.Write);
            StreamWriter streamWriter = new StreamWriter(fileStream);
            streamWriter.Write($"wget.exe -c -e ");
            bool hasHttps = URL.Text[4] == 's';
            bool hasUsername = Username.Text != "";
            bool hasPassword = Password.Text != "";
            bool hasSpeed = Speed.Text != "";
            if (hasHttps)
            {
                if(hasUsername)
                {
                    if(hasSpeed)
                    {
                        streamWriter.Write($"--limit-rate={Speed.Text} robots=off -x --user={Username.Text} --password={Password.Text} -m --no-check-certificate ");
                    }
                    else
                    {
                        streamWriter.Write($"robots=off -x --user={Username.Text} --password={Password.Text} -m --no-check-certificate ");
                    }
                }
                else
                {
                    if (hasSpeed)
                    {
                        streamWriter.Write($"--limit-rate={Speed.Text} robots=off -x -m --no-check-certificate ");
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
                        streamWriter.Write($"--limit-rate={Speed.Text} robots=off -x --user={Username.Text} --password={Password.Text} -m ");
                    }
                    else
                    {
                        streamWriter.Write($"robots=off -x --user={Username.Text} --password={Password.Text} -m ");
                    }
                }
                else
                {
                    if (hasSpeed)
                    {
                        streamWriter.Write($"--limit-rate={Speed.Text} robots=off -x -m ");
                    }
                    else
                    {
                        streamWriter.Write($"robots=off -x -m ");
                    }
                }
            }
            streamWriter.Write($"\"{URL.Text}\"");
            streamWriter.Close();
        }
        private void ProcExit(object sender, EventArgs e)                
        {
            if (!Result.Dispatcher.CheckAccess())
            {
                // Called from a none ui thread, so use dispatcher
                ChangeButtonDelegate changeButtonDelegate = new ChangeButtonDelegate(ChangeStatic);
                DownloadButton.Dispatcher.Invoke(DispatcherPriority.Normal, changeButtonDelegate);
            }
            else
            {
                // Called from UI trhead so just update the textbox
                ChangeStatic();
            }   
        }
        public void StartCmd()
        {
            ProcessStartInfo startInfo = new ProcessStartInfo
            {
                FileName = $"{SavePathTextBox.Text}\\DownloadVer.I.bat",
                Arguments = "2>&1",
                RedirectStandardError = true,
                RedirectStandardOutput = true,
                UseShellExecute = false,
                WorkingDirectory = SavePathTextBox.Text,
                CreateNoWindow = true
            };
            new Thread(() =>
            {
                using (Process process = new Process())
                {
                    process.StartInfo = startInfo;
                    process.OutputDataReceived += new DataReceivedEventHandler(ProcessOuputHandler);
                    process.EnableRaisingEvents = true;
                    process.Exited += new EventHandler(ProcExit);
                    process.Start();
                    process.BeginOutputReadLine();
                    while (!process.HasExited)
                    {
                        // Refresh WPF window here
                        Dispatcher.Invoke(DispatcherPriority.Render, EmptyDelegate);
                    }
                }
            }).Start();
        }
    }
}
