using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Diagnostics;
using System.IO;
using System.Xml;
using System.Xml.Linq;

namespace ENMV.I
{
    class DistributedEncoderMasterVerI
    {
        public static List<string> System = new List<string>();
        public static bool stat = false;
        public static string sour;
        public static string videoLong;
        public static short kbps;
        public static string fps;
        public static bool distributable = false;
        public static byte splitMode;
        static void Main(string[] args)
        {
            FileSystemWatcher watcher = new FileSystemWatcher
            {
                Path = AppDomain.CurrentDomain.SetupInformation.ApplicationBase,
                Filter = "Status.DBvI"
            };
            watcher.Deleted += new FileSystemEventHandler(Error1);
            watcher.Renamed += new RenamedEventHandler(Error2);
            watcher.Created += new FileSystemEventHandler(Error1);
            watcher.Changed += new FileSystemEventHandler(Change);
            watcher.IncludeSubdirectories = false;
            watcher.NotifyFilter = NotifyFilters.Attributes | NotifyFilters.CreationTime | NotifyFilters.DirectoryName | NotifyFilters.FileName | NotifyFilters.LastAccess | NotifyFilters.LastWrite | NotifyFilters.Security | NotifyFilters.Size;
            
            Console.WriteLine("Distributed Encoder Master Ver.I");
            Build();
            Build2(out System);
            watcher.EnableRaisingEvents = true;
            Start();
            if (stat)
            {
                watcher.EnableRaisingEvents = false;
                Accept();
                Merge();
                Console.WriteLine("[ ok ]");
                Console.ReadLine();
            }                      
        }
        private static void Change(object source, FileSystemEventArgs e)
        {
            Console.WriteLine("done.");
            StreamReader StatusDBvI = new StreamReader("Status.DBvI");
            List<string> status = new List<string>();
            while(true)
            {
                string inputRaw = StatusDBvI.ReadLine();
                if (inputRaw == null)
                    break;
                char[] sep = { '=' };
                foreach (string inputraw in inputRaw.Split(sep))
                {
                    if ((inputraw == "false") || (inputraw == "true"))
                        status.Add(inputraw);
                }
            }
            StatusDBvI.Close();
            foreach(string s in status)
            {
                if (s == "false")
                    break;
                stat = true;
            }
        }
        private static void Error2(object source, RenamedEventArgs e) => Console.WriteLine("Error:Status.DBvI has been renamed");
        private static void Error1(object source, FileSystemEventArgs e) => Console.WriteLine("Error:Status.DBvI has been deleted/created");
        public static void Build2(out List<string> System)
        {
            XmlDocument config = new XmlDocument();
            config.Load("Config.xml");
            System = new List<string>();
            XmlNodeList node = config.SelectNodes(@"//CADs");
            XmlElement element;
            List<string> system = new List<string>();

            foreach (XmlNode n in node)
                Console.WriteLine(n.OuterXml);
            for (int index = 1; true; ++index)
            {
                node = config.SelectNodes(@"//CADs/CAD[@Index='" + Convert.ToString(index) + "']");
                foreach (XmlNode n in node)
                    Console.WriteLine(n.OuterXml);
                element = (XmlElement)node[0];
                if (element == null)
                    break;
                Console.Write("use ");
                Console.Write(element.GetAttribute("ID"));
                Console.Write("?(y/N) ");
                if (Console.ReadLine() == "y")
                    system.Add(element.GetAttribute("ID"));
            }
            foreach (string s in system)
                System.Add(s);
        }
        public static bool Build()
        {
            if (!File.Exists("Config.xml"))
                XmlBuild.Create();
            const string Writeable = "Writeable";
            const string Info = "Info";
            const string Operation = "Operation";
            const string ID = "ID";
            const string Default = "default";
            const string Space = "Space";
            const string True = "true";
            const string This = "this";
            XmlDocument config = new XmlDocument();
            config.Load("Config.xml");

            XmlNodeList node = config.SelectNodes(@"//Paths");
            XmlElement element;
            List<string> path = new List<string>();

            foreach (XmlNode n in node)
                Console.WriteLine(n.OuterXml);
            for (int index = 1; true; ++index)
            {
                node = config.SelectNodes(@"//Paths/Path[@Index='" + Convert.ToString(index) + "']");

                element = (XmlElement)node[0];
                if (element == null)
                    break;
                path.Add(element.GetAttribute(ID));
                path.Add(element.InnerText);
            }





            string command = "";

            node = config.SelectNodes(@"//Functions");
            foreach (XmlNode n in node)
                Console.WriteLine(n.OuterXml);
            for (int index = 1; true; ++index)
            {
                node = config.SelectNodes(@"//Function[@Index=" + Convert.ToString(index) + "]");

                element = (XmlElement)node[0];
                if (element == null)
                    break;
                //Console.WriteLine(e.InnerText);
                Console.Write(index + " ");
                Console.WriteLine(element.GetAttribute(ID));
            }

            




            byte mode = Convert.ToByte(Console.ReadLine());

            node = config.SelectNodes(xpath: @"//Function[@Index='" + Convert.ToString(mode) + "']");
            element = (XmlElement)node[0];
            distributable = Convert.ToBoolean(element.GetAttribute("Distributed"));




            node = config.SelectNodes(@"//Function[@Index='" + Convert.ToString(mode) + "']/Key");
            foreach (XmlNode n in node)
                Console.WriteLine(n.OuterXml);
            //type=var
            List<double> var = new List<double>();
            for (int vary = 0; true; ++vary)
            {
                node = config.SelectNodes(@"//Function[@Index='" + Convert.ToString(mode) + "']/Key[@Type='var" + Convert.ToString(vary) + "']");
                foreach (XmlNode n in node)
                    Console.WriteLine(n.OuterXml);

                element = (XmlElement)node[0];
                if (element == null)
                    break;

                Console.Write(element.GetAttribute(Info) + "(default=" + element.InnerText + ")=");
                string input = Console.ReadLine();
                if (input == "")
                    var.Add(Convert.ToDouble(element.InnerText));
                else
                    var.Add(Convert.ToDouble(input));
            }

            //type=key
            for (int index = 1; true; ++index)
            {
                string value = "";
                node = config.SelectNodes(@"//Function[@Index='" + Convert.ToString(mode) + "']/Key[@Index='" + Convert.ToString(index) + "']");
                foreach (XmlNode n in node)
                    Console.WriteLine(n.OuterXml);
                element = (XmlElement)node[0];
                if (element == null)
                    break;
                if (element.GetAttribute(Writeable) == True)
                {
                    Console.Write(element.GetAttribute(Info) + "(" + Default + "=" + element.InnerText + ")=");
                    string input = Console.ReadLine();
                    if (input == "")
                        value = element.InnerText;
                    else
                        value = input;
                    if (element.GetAttribute(Info) == "input path")
                        sour = value;
                    if (element.GetAttribute(Info) == "input FPS")
                        fps = value;
                }
                else
                {
                    value += element.InnerText;
                }
                if (element.GetAttribute(Operation) != "")
                {
                    string operationRaw = element.GetAttribute(Operation);
                    List<string> operation = new List<string>();
                    char[] space = { ' ' };
                    foreach (string o in operationRaw.Split(space))
                        operation.Add(o);
                    for (int inde = 0; inde < operation.Count; ++inde)
                        for (int ind = 0; ind < path.Count; ++ind)
                            if (operation[inde] == path[ind] && (ind % 2 == 0 || ind == 0))
                                operation[inde] = path[ind + 1];
                    for (int inde = 0; inde < operation.Count; ++inde)
                        if (operation[inde] == This)
                            operation[inde] = value;
                    for (int inde = 0; inde < operation.Count; ++inde)
                        for (int ind = 0; true; ++ind)
                            if (operation[inde] == ("var" + Convert.ToString(ind)))
                                operation[inde] = Convert.ToString(var[ind]);
                            else
                                break;
                    for (int inde = 0; inde < operation.Count; ++inde)
                    {
                        if (operation[inde] == "*")
                        {
                            operation[inde - 1] = Convert.ToString(Convert.ToDouble(operation[inde - 1]) * Convert.ToDouble(operation[inde + 1]));
                            operation.RemoveAt(inde);
                            operation.RemoveAt(inde);
                        }
                        else if (operation[inde] == "/")
                        {
                            operation[inde - 1] = Convert.ToString(Convert.ToDouble(operation[inde - 1]) / Convert.ToDouble(operation[inde + 1]));
                            operation.RemoveAt(inde);
                            operation.RemoveAt(inde);
                        }
                    }
                    for (int inde = 0; inde < operation.Count; ++inde)
                    {
                        if (operation[inde] == "+")
                        {
                            operation[inde - 1] = Convert.ToString(Convert.ToDouble(operation[inde - 1]) + Convert.ToDouble(operation[inde + 1]));
                            operation.RemoveAt(inde);
                            operation.RemoveAt(inde);
                        }
                        else if (operation[inde] == "-")
                        {
                            operation[inde - 1] = Convert.ToString(Convert.ToDouble(operation[inde - 1]) - Convert.ToDouble(operation[inde + 1]));
                            operation.RemoveAt(inde);
                            operation.RemoveAt(inde);
                        }
                    }
                    value = operation[0];

                }
                if (element.GetAttribute(Space) == True)
                    value += " ";
                command += value;
            }
            StreamWriter swbat = new StreamWriter("Distributed Encoder Master Ver.I.bat");
            swbat.WriteLine(command);


            for (int cmd = 0; true; ++cmd)
            {
                node = config.SelectNodes(xpath: @"//Function[@Index='" + Convert.ToString(mode) + "']/Key[@Type='cmd" + Convert.ToString(cmd) + "']");
                foreach (XmlNode n in node)
                    Console.WriteLine(n.OuterXml);

                element = (XmlElement)node[0];
                if (element == null)
                    break;
                string sp = element.InnerText;
                swbat.WriteLine(element.InnerText);
            }

            swbat.Close();
            return true;
        }
        public static void Start()
        {
            Split();
            Send();
            Process pbat = new Process();
            pbat.StartInfo.FileName = "Distributed Encoder Master Ver.I.bat";
            pbat.Start();
            pbat.WaitForExit();
            pbat.Close();
        }
        public static void Split()
        {
            if (distributable)
            {
                StreamWriter SplitBat = new StreamWriter("split.bat");
                XmlDocument config = new XmlDocument();
                config.Load("Config.xml");

                XmlNodeList node = config.SelectNodes(@"//Paths");
                XmlElement element;
                Console.WriteLine("Split mode:1-by time 2-by frame");
                splitMode = Convert.ToByte(Console.ReadLine());
                if (splitMode == 1)
                {
                    
                }
                else if (splitMode == 2)
                {
                    foreach (XmlNode n in node)
                        Console.WriteLine(n.OuterXml);
                    node = config.SelectNodes(@"//Paths/Path[@ID='FFmpeg']");
                    element = (XmlElement)node[0];
                    SplitBat.WriteLine(element.InnerText + " -i " + sour + AppDomain.CurrentDomain.SetupInformation.ApplicationBase + "%d.jpg");
                    SplitBat.Close();
                    Process pbat = new Process();
                    pbat.StartInfo.FileName = "split.bat";
                    pbat.Start();
                    pbat.WaitForExit();
                    pbat.Close();
                }
            }
        }
        public static void Merge()
        {

        }
        public static void Send()
        {
            if(splitMode==1)
            {

            }
            if (splitMode == 2)
            {
                long frame = 1;
                while (true)
                {
                    if (File.Exists(Convert.ToString(frame) + ".jpg") == false)
                        break;
                    ++frame;
                }
                List<double> performance = new List<double>();
                List<long> keyFrame = new List<long>();
                XmlDocument config = new XmlDocument();
                config.Load("Config.xml");
                XmlNodeList node = config.SelectNodes(@"//Paths");
                XmlElement element;
                for (int index=0;index<System.Count;++index)
                {
                    node = config.SelectNodes(xpath: @"//CAD[ID='" + System[index] + "'");
                    element = (XmlElement)node[0];
                    performance.Add(Convert.ToDouble(element.GetAttribute("Performance")));
                }
                for (double part = 0, index = 0; index < System.Count; ++index)
                {
                    part += performance[Convert.ToInt32(index)];
                    keyFrame.Add(Convert.ToInt64(frame * part / performance.Sum()));
                }
                StreamWriter send = new StreamWriter("Send.bat"); 
                for(int c=0;c<System.Count;++c)
                {
                    for(long f=1;f<frame;++f)
                    {
                        
                    }
                }
            }
        }
        public static void Accept()
        {

        }
    }
    /*
    public class Bat
    {
        public void Run(byte mode)
        {
            string input = Console.ReadLine();
            string output = Console.ReadLine();

            StreamWriter swbat = new StreamWriter("df.bat");
            swbat.WriteLine();
            swbat.Close();
            Process pbat = new Process();
            pbat.StartInfo.FileName = "df.bat";
            pbat.Start();
            pbat.WaitForExit();
            pbat.Close();
        }
    }
    */
    public static class XmlBuild
    {
        public static void Create()
        {
            XmlDocument config = new XmlDocument();
            XmlNode node = config.CreateXmlDeclaration("1.0", "utf-8", "");
            config.AppendChild(node);
            XmlNode root = config.CreateElement("DistributedEncoderMasterConfigVer.I");
            config.AppendChild(root);

            XmlNode path = config.CreateNode(XmlNodeType.Element, "Path", null);
            Console.Write("FFmpeg path = ");
            CreateNode(config, path, "FFmpeg", Console.ReadLine());
            root.AppendChild(path);

            config.Save("Config.xml");
        }
        public static void CreateNode(XmlDocument xmlDoc, XmlNode parentNode, string name, string value)
        {
            XmlNode node = xmlDoc.CreateNode(XmlNodeType.Element, name, null);
            node.InnerText = value;
            parentNode.AppendChild(node);
        }
    } 
}
