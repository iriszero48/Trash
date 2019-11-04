using System;
using System.Collections.Generic;
using System.IO;
using System.Diagnostics;
using System.Windows.Forms;
using System.Drawing;
using System.Runtime.InteropServices;
using System.Text;
using System.Xml;
using System.Reflection;

namespace Qiniu_Sync_Ver.I
{
    internal class Qsvi
    {
        private static NotifyIcon notifyIcon = new NotifyIcon();
        private static bool Visible = true;

        private static void Main(string[] args)
        {
            notifyIcon.DoubleClick += (s, e) =>
            {
                Visible = !Visible;
                SetConsoleWindowVisibility(Visible);
            };
            notifyIcon.Icon = Icon.ExtractAssociatedIcon(Application.ExecutablePath);
            List<Task> tasks = new List<Task>();
            notifyIcon.Visible = true;
            notifyIcon.Text = Application.ProductName;
            new ContextMenuStrip().Items.Add(text: "Exit", image: null, onClick: (s, e) => { Application.Exit(); });
            notifyIcon.ContextMenuStrip = new ContextMenuStrip();
            Console.WriteLine("Qiniu Sync Ver.I");
            DarkGreen("Qiniu Sync Ver.I:/# ");
            Console.WriteLine("-l");
            Login();
            ShowBuckets();
            ShowHelp();
            UI(ref tasks);
            List<FileSystemWatcher> sync = new List<FileSystemWatcher>();
            Application.Run();
            notifyIcon.Visible = false;
        }

        private static void AddTask()
        {
            Console.Write("LocalPath: ");
            StringBuilder LocalPath = new StringBuilder(Console.ReadLine());
            if (LocalPath[0] == '"') 
            {
                LocalPath.Remove(LocalPath.Length - 1, 1);
            }
            if(LocalPath[LocalPath.Length-1]=='"')
            {
                LocalPath.Remove(LocalPath.Length - 1, 1);
            }
            if (LocalPath[LocalPath.Length - 1] == '\\') 
            {
                LocalPath.Remove(LocalPath.Length - 1, 1);
            }
            Console.Write("Bucket: ");
            string Bucket = Console.ReadLine();
            string xmlPath = ConfigPath();
            XmlDocument config = new XmlDocument();
            config.Load(xmlPath);
            XmlNode newNode = config.CreateNode(nodeTypeString: "element", name: "Task", namespaceURI: "");
            newNode.InnerText = LocalPath.ToString();
            ((XmlElement)newNode).SetAttribute(name: "ID", value: TaskLength(config).ToString());
            ((XmlElement)newNode).SetAttribute(name: "Bucket", value: Bucket);
            config.DocumentElement.AppendChild(newChild: newNode);
            config.Save(filename: xmlPath);
        }

        private static string ConfigPath() => $"{Path()}config\\Qiniu Sync Ver.I.xml";

        private static int TaskLength(XmlDocument config) => config.SelectNodes(xpath: @"//Task").Count;

        private static void UI(ref List<Task> tasks)
        {
            bool disexit = true;
            while (disexit)
            {
                DarkGreen("Qiniu Sync Ver.I:/# ");
                string input = Console.ReadLine();
                switch (input)
                {
                    case "-a":
                        AddTask();
                        break;
                    case "-l":
                        Login();
                        break;
                    case "-s":
                        disexit = false;
                        StratSync(ref tasks);
                        break;
                    case "-d-b":
                        ShowBuckets();
                        break;
                    case "-r":

                        break;
                    case "-h":
                        ShowHelp();
                        break;
                    case "-d-t":

                        break;
                    default:
                        DarkRed("[!] Qiniu Sync Ver.I");
                        Console.WriteLine(": No argument");
                        break;
                }
            }
        }

        private static void DarkRed(string console)
        {
            Console.ForegroundColor = ConsoleColor.DarkRed;
            Console.Write(console);
            Console.ForegroundColor = ConsoleColor.White;
        }

        private static void DarkGreen(string console)
        {
            Console.ForegroundColor = ConsoleColor.DarkGreen;
            Console.Write(console);
            Console.ForegroundColor = ConsoleColor.White;
        }

        private static void ShowHelp()
        {
            Console.ForegroundColor = ConsoleColor.DarkBlue;
            Console.Write(value: $"[*] {Qscvi()}");
            Console.ForegroundColor = ConsoleColor.White;
            Console.WriteLine(": -h");
            Console.WriteLine("  Examples :");
            Console.WriteLine("");
            Console.WriteLine("    Add sync task ...");
            Console.WriteLine("    -a");
            Console.WriteLine("");
            Console.WriteLine("    Login account ...");
            Console.WriteLine("    -l");
            Console.WriteLine("");
            Console.WriteLine("    Start sync tasks ...");
            Console.WriteLine("    -s");
            Console.WriteLine("");
            Console.WriteLine("    Remove sync task ...");       
            Console.WriteLine("    -r");
            Console.WriteLine("");
            Console.WriteLine("    Display sync tasks ...");
            Console.WriteLine("    -d-t");
            Console.WriteLine("");
            Console.WriteLine("    Display buckets");
            Console.WriteLine("    -d-b");
            Console.WriteLine("");
        }

        private static void StratSync(ref List<Task> tasks)
        {
            tasks = new List<Task>();
            XmlDocument config = new XmlDocument();
            config.Load(ConfigPath());
            for (int i=0;i<TaskLength(config);++i)
            {
                tasks.Add(new Task(i.ToString()));
            }
            for(int i=0;i<tasks.Count;++i)
            {
                tasks[i].StartSync();
            }
        }
        private static void Login()
        {
            DarkMagenta("Login from [0]AccessKey [1]User: ");
            try
            {
                if(Convert.ToBoolean(Convert.ToByte(Console.ReadLine())))
                {
                    LogicByUser();
                }
                else
                {
                    LogicByAccessKey();
                }
            }
            catch
            {
                Login();
            }
        }

        private static void LogicByAccessKey(string ak = null, string sk = null)
        {
            if (ak == null || sk == null)
            {
                DarkMagenta("AccessKey");
                Console.Write(": ");
                ak = Console.ReadLine();
                DarkMagenta("SecretKey");
                Console.Write(": ");
                sk = Console.ReadLine();
            }
            QshellCmd(cmd: $"account {ak} {sk}");
        }

        private static void LogicByUser()
        {
            DarkMagenta("User");
            Console.Write(": ");
            string user = Console.ReadLine();
            DarkMagenta("Passwd");
            Console.Write(": ");
            string pw = Console.ReadLine();
            QrsctlCmd($"login {user} {pw}");
            QrsctlCmd(cmd: $"appinfo default", isLogin: false);
        }

        private static void DarkMagenta(string console)
        {
            Console.ForegroundColor = ConsoleColor.DarkMagenta;
            Console.Write(console);
            Console.ForegroundColor = ConsoleColor.White;
        }

        public static void QshellCmd(string cmd, bool enableOutput = true) => Cmd(cmd: $"\"{Path()}bin\\qshell-windows-x64.exe\" {cmd}", enableOutput: enableOutput);
        public static string QrsctlCmd(string cmd, bool enableOutput = true, bool isLogin = false) => Cmd(cmd: $"\"{Path()}bin\\qrsctl-v3.2.20170501.exe\" {cmd}", enableOutput: enableOutput);

        private static string Cmd(string cmd, bool enableOutput = true, bool isLogin = false)
        {
            string ans = "";
            Process cmdProcess = new Process();
            cmdProcess.StartInfo.FileName = "cmd.exe";
            cmdProcess.StartInfo.UseShellExecute = false;
            cmdProcess.StartInfo.RedirectStandardInput = true;
            cmdProcess.StartInfo.RedirectStandardOutput = true;
            cmdProcess.StartInfo.RedirectStandardError = true;
            cmdProcess.StartInfo.CreateNoWindow = true;
            cmdProcess.Start();
            cmdProcess.StandardInput.WriteLine(cmd + " &exit");
            cmdProcess.StandardInput.AutoFlush = true;
            StringBuilder output = new StringBuilder(value: cmdProcess.StandardOutput.ReadToEnd());
            cmdProcess.WaitForExit();
            cmdProcess.Close();
            if (enableOutput)
            {
                for (int d = 0; d < 2; output.Remove(startIndex: 0, length: 1)) 
                {
                    if (output[index: 0] == '\"')
                    {
                        ++d;
                    }
                }
                output.Remove(startIndex: 0, length: 1);
                List<StringBuilder> outputStringList = new List<StringBuilder>();
                foreach (string o in output.ToString().Split(separator: '\n'))
                {
                    outputStringList.Add(item: new StringBuilder(value: o));
                }              
                for (int i = 1; i < outputStringList.Count; ++i)
                {
                    outputStringList[index: i].Insert(index: 0, value: $"[*] {outputStringList[0].ToString().Split(' ')[0]}: ");
                }
                outputStringList.RemoveAt(index: outputStringList.Count - 1);
                for (int i = 0; i < 7; ++i)
                {
                    outputStringList[index: 0].Remove(startIndex: outputStringList[0].Length - 1, length: 1);
                }
                outputStringList[0].Insert(0, value: $"[*] {Qscvi()}: ");
                if (isLogin)
                {
                    ans = $"{outputStringList[1].ToString().Split(':')[1]} {outputStringList[2].ToString().Split(':')[1]}";
                }
                for (int i=0;i<outputStringList.Count;++i)
                {
                    Console.ForegroundColor = ConsoleColor.DarkBlue;
                    Console.Write(outputStringList[i].ToString().Split(':')[0]);
                    Console.ForegroundColor = ConsoleColor.White;
                    for(int j=1;j<outputStringList[i].ToString().Split(':').Length;++j)
                    {
                        Console.Write(":");
                        if(j== outputStringList[i].ToString().Split(':').Length-1)
                        {
                            Console.WriteLine(outputStringList[i].ToString().Split(':')[j]);
                        }
                        else
                        {
                            Console.Write(outputStringList[i].ToString().Split(':')[j]);
                        }
                    }
                }
            }
            return ans;
        }
        

        [DllImport(dllName: "user32.dll")]
        public static extern IntPtr FindWindow(string lpClassName, string lpWindowName);
        [DllImport(dllName: "user32.dll")]
        private static extern bool ShowWindow(IntPtr hWnd, int nCmdShow);
        public static void SetConsoleWindowVisibility(bool visible)
        {
            IntPtr hWnd = FindWindow(lpClassName: null, lpWindowName: Console.Title);
            if (hWnd != IntPtr.Zero)
            {
                if (visible)
                {
                    ShowWindow(hWnd: hWnd, nCmdShow: 1);
                }
                else 
                {
                    ShowWindow(hWnd: hWnd, nCmdShow: 0);             
                }
            }
        }

        private static string Qscvi() => "Qiniu Sync Ver.I";
        private static void ShowBuckets() => QshellCmd(cmd: "buckets");
        public static string Path() => AppDomain.CurrentDomain.SetupInformation.ApplicationBase;
        public static string GetAttribute(string ID,string Attribute)
        {
            XmlDocument config = new XmlDocument();
            config.Load(ConfigPath());
            return ((XmlElement)config.SelectNodes(xpath: @"//Task[@ID='" + ID + "']")[i: 0]).GetAttribute(name: Attribute);
        }
        public static string GetInnerText(string ID)
        {
            XmlDocument config = new XmlDocument();
            config.Load(ConfigPath());
            return ((XmlElement)config.SelectNodes(xpath: @"//Task[@ID='" + ID + "']")[i: 0]).InnerText;
        }
        public static string Bucket() => "Bucket";
    }
    public class Task
    {
        private string ID;
        private FileSystemWatcher syncDir;

        public Task(string ID)
        {
            this.ID = ID;
            syncDir = new FileSystemWatcher()
            {
                Path = Qsvi.GetInnerText(ID),
                Filter = "*.*"
            };
            syncDir.Changed += new FileSystemEventHandler(OnProcess);
            syncDir.Created += new FileSystemEventHandler(OnProcess);
            syncDir.Deleted += new FileSystemEventHandler(OnProcess);
            syncDir.Renamed += new RenamedEventHandler(OnRenamed);
            syncDir.NotifyFilter = NotifyFilters.Attributes | NotifyFilters.CreationTime | NotifyFilters.DirectoryName | NotifyFilters.FileName | NotifyFilters.LastAccess | NotifyFilters.LastWrite | NotifyFilters.Security | NotifyFilters.Size;
            syncDir.IncludeSubdirectories = true;
        }
        public void StartSync()
        {
            Output();
            Sync();
            syncDir.EnableRaisingEvents = true;
        }

        private  void Sync() => Qsvi.QshellCmd($"qupload \"{Qsvi.Path()}config\\qupload.{ID.GetHashCode()}.config\"");
        private string Src_dir(bool content) => content ? $"\"{Qsvi.GetInnerText(ID)}\"" : MethodBase.GetCurrentMethod().Name.ToLower();
        private string Bucket(bool content) => content ? $"\"{Qsvi.GetAttribute(ID, MethodBase.GetCurrentMethod().Name)}\"" : MethodBase.GetCurrentMethod().Name.ToLower();
        private string Ignore_dir(bool content) => content ? false.ToString().ToLower() : MethodBase.GetCurrentMethod().Name.ToLower();
        private string Overwrite(bool content) => content ? true.ToString().ToLower() : MethodBase.GetCurrentMethod().Name.ToLower();
        private string Check_exists(bool content) => content ? true.ToString().ToLower() : MethodBase.GetCurrentMethod().Name.ToLower();
        private string Check_hash(bool content) => content ? true.ToString().ToLower() : MethodBase.GetCurrentMethod().Name.ToLower();
        private string Check_size(bool content) => content ? false.ToString().ToLower() : MethodBase.GetCurrentMethod().Name.ToLower();
        private string Rescan_local(bool content) => content ? true.ToString().ToLower() : MethodBase.GetCurrentMethod().Name.ToLower();
        private string Log_file(bool content) => content ? $"\"qupload.log\"" : MethodBase.GetCurrentMethod().Name.ToLower();
        private string Log_level(bool content) => content ? $"\"debug\"" : MethodBase.GetCurrentMethod().Name.ToLower();
        private string Log_rotate(bool content) => content ? "1" : MethodBase.GetCurrentMethod().Name.ToLower();
        private string Log_stdout(bool content) => content ? true.ToString().ToLower() : MethodBase.GetCurrentMethod().Name.ToLower();
        private string File_type(bool content) => content ? "0" : MethodBase.GetCurrentMethod().Name.ToLower();
        private string ConfigPath() => $"{Qsvi.Path()}config\\qupload.{ID.GetHashCode()}.config";
        private string Doublize(string raw)
        {
            StringBuilder input = new StringBuilder(raw);
            for(int i=0;i<input.Length;++i)
            {
                if(input[i]=='\\')
                {
                    input.Insert(i, '\\');
                    ++i;
                }
            }
            return input.ToString();
        }
        private void Output()
        {
            StreamWriter writeConfig = new StreamWriter(stream: new FileStream(ConfigPath(), FileMode.Create, FileAccess.Write, FileShare.ReadWrite));
            writeConfig.WriteLine("{");
            writeConfig.WriteLine($"   \"{Src_dir(false)}\":{Doublize(Src_dir(true))},");
            writeConfig.WriteLine($"   \"{Bucket(false)}\":{Bucket(true)},");
            writeConfig.WriteLine($"   \"{Ignore_dir(false)}\":{Ignore_dir(true)},");
            writeConfig.WriteLine($"   \"{Overwrite(false)}\":{Overwrite(true)},");
            writeConfig.WriteLine($"   \"{Check_exists(false)}\":{Check_exists(true)},");
            writeConfig.WriteLine($"   \"{Check_hash(false)}\":{Check_hash(true)},");
            writeConfig.WriteLine($"   \"{Check_size(false)}\":{Check_size(true)},");
            writeConfig.WriteLine($"   \"{Rescan_local(false)}\":{Rescan_local(true)},");
            writeConfig.WriteLine($"   \"{Log_file(false)}\":{Log_file(true)},");
            writeConfig.WriteLine($"   \"{Log_level(false)}\":{Log_level(true)},");
            writeConfig.WriteLine($"   \"{Log_rotate(false)}\":{Log_rotate(true)},");
            writeConfig.WriteLine($"   \"{Log_stdout(false)}\":{Log_stdout(true)},");
            writeConfig.WriteLine($"   \"{File_type(false)}\":{File_type(true)}");
            writeConfig.WriteLine("}");
            writeConfig.Close();
        }
        private void OnProcess(object source, FileSystemEventArgs e)
        {         
            if (e.ChangeType == WatcherChangeTypes.Created)
            {
                Console.ForegroundColor = ConsoleColor.DarkBlue;
                Console.Write($"[*] File {e.ChangeType}");
                Console.ForegroundColor = ConsoleColor.White;
                Console.WriteLine($": {e.FullPath}");
                OnCreated(source: source, e: e);

            }
            else if (e.ChangeType == WatcherChangeTypes.Changed)
            {
                Console.ForegroundColor = ConsoleColor.DarkBlue;
                Console.Write($"[*] File {e.ChangeType}");
                Console.ForegroundColor = ConsoleColor.White;
                Console.WriteLine($": {e.FullPath}");
                OnChanged(source: source, e: e);

            }
            else if (e.ChangeType == WatcherChangeTypes.Deleted)
            {
                Console.ForegroundColor = ConsoleColor.DarkRed;
                Console.Write($"[!] File {e.ChangeType}");
                Console.ForegroundColor = ConsoleColor.White;
                Console.WriteLine($": {e.FullPath}");
                //OnDeleted(source: source, e: e);
            }
        }
        private void OnProcess(object source,RenamedEventArgs e)
        {
            Console.ForegroundColor = ConsoleColor.DarkBlue;
            Console.Write($"[*] File {e.ChangeType}");
            Console.ForegroundColor = ConsoleColor.White;
            Console.WriteLine($": {e.OldFullPath} => {e.FullPath}");
            OnRenamed(source, e);
        }
        private void OnCreated(object source, FileSystemEventArgs e) => Sync();
        private void OnChanged(object source, FileSystemEventArgs e) => Sync();
        //private void OnDeleted(object source, FileSystemEventArgs e) => ;
        private void OnRenamed(object source, RenamedEventArgs e) => Sync();
    }
}
