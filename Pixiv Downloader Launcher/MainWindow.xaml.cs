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
using System.IO;
using System.IO.Compression;
using System.Xml;

namespace Pixiv_Downloader_Launcher
{
    /// <summary>
    /// MainWindow.xaml 的交互逻辑
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
        }
        private string SettingFilename() => "PixivDownloaderLauncher.Config.xml";
        private bool SaveSetting(Setting setting) => setting.Save(SettingFilename());
        private void IDDownloadButton_Click(object sender, RoutedEventArgs e)
        {
            if(SettingIsNull())
            {
                MessageBox.Show("未设置", "致命错误", MessageBoxButton.OK, MessageBoxImage.Exclamation);
            }
            else
            {

            }
        }
        private void SaveSettingButton_Click(object sender, RoutedEventArgs e)
        {
            if (SettingIsNull())
            {
                MessageBox.Show("未设置", "致命错误", MessageBoxButton.OK, MessageBoxImage.Exclamation);
            }
            else
            {
                SaveSetting(new Setting(UsernameTextBox.Text, PasswordTextBox.Text, PixivDownloaderPathTextBox.Text, ChromeDriverPathTextBox.Text));
            }
        }
    }
    public class Setting
    {
        private string _username;
        private string _password;
        private string _pixivDownloaderPyPath;
        private string _chromedriverExePath;
        public string Username
        {
            get => _username;
            set => _username = value;
        }
        public string Password
        {
            get => DeflateDecompress(_password);
            set
            {
                if (value == "")
                {
                    throw new ArgumentNullException();
                }
                else
                {
                    _password = DeflateCompress(value);
                }
            }
        }
        public string PixivDownloaderPyPath
        {
            get => _pixivDownloaderPyPath;
            set => _pixivDownloaderPyPath = value;
        }
        public string ChromedriverExePath
        {
            get => _chromedriverExePath;
            set => _chromedriverExePath = value;
        }
        public Setting(string Username, string Password, string PixivDownloaderPyPath, string ChromedriverExePath)
        {
            this.Username = Username;
            this.Password = Password;
            this.PixivDownloaderPyPath = PixivDownloaderPyPath;
            this.ChromedriverExePath = ChromedriverExePath;
        }
        public Setting(string SettingFilePath)
        {

        }
        private bool SettingIsNull() => Username == "" || Password == "" || PixivDownloaderPyPath == "" || ChromedriverExePath == "" ? true : false;
        private string DeflateDecompress(string strSource)
        {
            byte[] buffer = Convert.FromBase64String(strSource);
            using (MemoryStream ms = new MemoryStream())
            {
                ms.Write(buffer, 0, buffer.Length);
                ms.Position = 0;
                using (DeflateStream stream = new DeflateStream(ms, CompressionMode.Decompress))
                {
                    stream.Flush();
                    int nSize = 16 * 1024 + 256;
                    byte[] decompressBuffer = new byte[nSize];
                    int nSizeIncept = stream.Read(decompressBuffer, 0, nSize);
                    stream.Close();
                    return Encoding.UTF8.GetString(decompressBuffer, 0, nSizeIncept);
                }
            }
        }
        private string DeflateCompress(string strSource)
        {
            byte[] buffer = Encoding.UTF8.GetBytes(strSource);
            using (MemoryStream ms = new MemoryStream())
            {
                using (DeflateStream stream = new DeflateStream(ms, CompressionMode.Compress, true))
                {
                    stream.Write(buffer, 0, buffer.Length);
                    stream.Close();
                }
                byte[] compressedData = ms.ToArray();
                ms.Close();
                return Convert.ToBase64String(compressedData);
            }
        }
        public bool Save(string SettingFilename)
        {
            if (SettingIsNull())
            {
                return false;
            }
            if (File.Exists(SettingFilename))
            {
                //setting.Save();
            }
            return true;
        }
    }
    public static class OneNodeXML
    {
        
    }
}
