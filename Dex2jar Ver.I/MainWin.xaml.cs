using System.Windows;
using Microsoft.WindowsAPICodePack.Dialogs;
using System.IO;
using System.Xml;
using System.Diagnostics;

namespace Dex2jar_Ver.I
{
    /// <summary>
    /// MainWindow.xaml 的交互逻辑
    /// </summary>
    public partial class MainWindow : Window
    {
        private string Config() => "Dex2jarVer.IConfig.xml";
        public MainWindow()
        {
            InitializeComponent();
            dex2jarPath.Text = "";
            inputPath.Text = "";
            outputPath.Text = "";
            if(File.Exists(Config()))
            {
                XmlDocument config = new XmlDocument();
                config.Load(Config());
                dex2jarPath.Text= ((XmlElement)config.SelectNodes(xpath: @"//Dex2jarPath")[i: 0]).InnerText;
            }
        }
        private void Dex2jarPath(object sender, RoutedEventArgs e)
        {
            var dialog = new CommonOpenFileDialog
            {
                IsFolderPicker = true
            };
            if (dialog.ShowDialog() == CommonFileDialogResult.Ok)
            {
                dex2jarPath.Text = dialog.FileName;
                if (File.Exists(Config()))
                {
                    XmlDocument config = new XmlDocument();
                    config.Load(Config());
                    ((XmlElement)config.SelectNodes(xpath: @"//Dex2jarPath")[i: 0]).InnerText = inputPath.Text;
                }
                else
                {

                    XmlDocument config1 = new XmlDocument();
                    XmlNode node = config1.CreateXmlDeclaration("1.0", "utf-8", "");
                    config1.AppendChild(node);
                    XmlNode root = config1.CreateElement(Config());
                    config1.AppendChild(root);
                    config1.Save(Config());

                    XmlDocument config2 = new XmlDocument();
                    config2.Load(Config());
                    XmlNode newNode = config2.CreateNode(nodeTypeString: "element", name: "Dex2jarPath", namespaceURI: "");
                    newNode.InnerText = dex2jarPath.Text;
                    config2.DocumentElement.AppendChild(newChild: newNode);
                    config2.Save(filename: Config());
                }
            }
        }

        private void InputPath(object sender, RoutedEventArgs e)
        {
            Microsoft.Win32.OpenFileDialog openFileDialog = new Microsoft.Win32.OpenFileDialog
            {
                DefaultExt = "*.dex",
                Filter = "(*.dex)|*.dex"
            };
            if (openFileDialog.ShowDialog() == true)
            {
                inputPath.Text = openFileDialog.FileName;
                var sp = inputPath.Text.Split('\\');
                outputPath.Text = "";
                for (short i=0;i<sp.Length-1;++i)
                {
                    outputPath.Text += sp[i];
                    outputPath.Text += "\\";
                }
                outputPath.Text += "classes-dex2jar.jar";
            }
        }

        private void OutputPath(object sender, RoutedEventArgs e)
        {
            Microsoft.Win32.SaveFileDialog saveFileDialog = new Microsoft.Win32.SaveFileDialog
            {
                DefaultExt = "*.jar",
                Filter = "(*.jar)|*.jar"
            };
            if (saveFileDialog.ShowDialog() == true)
            {
                outputPath.Text = saveFileDialog.FileName;
            }
        }

        private void StartConvert(object sender, RoutedEventArgs e)
        {
            if (dex2jarPath.Text != "" && inputPath.Text != "" && outputPath.Text != "")
            {
                Process cmdProcess = new Process();
                cmdProcess.StartInfo.FileName = "cmd.exe";
                cmdProcess.StartInfo.UseShellExecute = false;
                cmdProcess.StartInfo.RedirectStandardInput = true;
                cmdProcess.StartInfo.RedirectStandardOutput = true;
                cmdProcess.StartInfo.RedirectStandardError = true;
                cmdProcess.StartInfo.CreateNoWindow = true;
                cmdProcess.Start();
                cmdProcess.StandardInput.WriteLine($"\"{dex2jarPath.Text}\\d2j-dex2jar.bat\" \"{inputPath.Text}\" &exit");
                cmdProcess.StandardInput.AutoFlush = true;
                string output = cmdProcess.StandardOutput.ReadToEnd();
                cmdProcess.WaitForExit();
                cmdProcess.Close();
                if(File.Exists(outputPath.Text))
                {
                    File.Delete(outputPath.Text);
                }
                File.Move("classes-dex2jar.jar", outputPath.Text);
                Result.Text = $"{output}dex2jar {inputPath.Text} -> {outputPath.Text}\nDone.\n";
            }
        }
    }
}
