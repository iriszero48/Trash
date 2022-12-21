using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
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
using System.Net.Http;
using System.Text.RegularExpressions;
using System.Threading;
using Path = System.IO.Path;
using Microsoft.Playwright;

namespace fk_kugou
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();

            _downloading = false;
            Environment.SetEnvironmentVariable("PLAYWRIGHT_BROWSERS_PATH", "pw-browsers", EnvironmentVariableTarget.Process);
        }

        private bool _downloading;

        static string DownDir = "下载";

        private void Println(string info)
        {
            Application.Current.Dispatcher.Invoke(() => DownLog.Text += $"{info}\n");
        }

        private async void DownImpl(string url)
        {
            try
            {
                using var playwright = await Playwright.CreateAsync();
                await using var browser = await playwright.Chromium.LaunchAsync();
                var page = await browser.NewPageAsync();
                await page.GotoAsync(url);
                var audio = await page.Locator("#myAudio").GetAttributeAsync("src");
                if (audio is null)
                {
                    MessageBox.Show("抓取错误", "错误", MessageBoxButton.OK, MessageBoxImage.Error);
                    _downloading = false;
                    return;
                }

                var filename = await page.Locator("#songNameTemp").GetAttributeAsync("title");
                if (filename is null) Println("获取歌名失败");
                Println($"开始下载: {audio}");

                using (var wc = new HttpClient())
                {
                    var response = await wc.GetAsync(audio);
                    var safeFilename = Path.GetInvalidFileNameChars().Aggregate(filename ?? "未命名", (a, x) => a.Replace(x, '_'));
                    var outPath = Path.Combine(DownDir, $"{safeFilename}.mp3");
                    if (!Directory.Exists(DownDir)) Directory.CreateDirectory(DownDir);
                    await using var fs = new FileStream(outPath, FileMode.OpenOrCreate);
                    await response.Content.CopyToAsync(fs);

                    Println($"保存到: {outPath}");
                }

                Println("下载完成");
                MessageBox.Show("下载成功");
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, "错误", MessageBoxButton.OK, MessageBoxImage.Error);
            }

            Application.Current.Dispatcher.Invoke(() => _downloading = false);
        }

        private async void DownBtn_Click(object sender, RoutedEventArgs e)
        {
            if (_downloading)
            {
                MessageBox.Show("正在下载", "提示", MessageBoxButton.OK, MessageBoxImage.Warning);
                return;
            }

            var url = DownInput.Text;
            if (string.IsNullOrWhiteSpace(url))
            {
                MessageBox.Show("请输入链接", "提示", MessageBoxButton.OK, MessageBoxImage.Warning);
                return;
            }

            if (!(Uri.TryCreate(url, UriKind.Absolute, out var uriResult)
                && (uriResult.Scheme == Uri.UriSchemeHttp || uriResult.Scheme == Uri.UriSchemeHttps)))
            {
                MessageBox.Show("无效链接", "提示", MessageBoxButton.OK, MessageBoxImage.Warning);
                return;
            }

            _downloading = true;
            Println($"开始抓取: {url}");

            await Task.Run(() => DownImpl(url));
        }
    }
}
