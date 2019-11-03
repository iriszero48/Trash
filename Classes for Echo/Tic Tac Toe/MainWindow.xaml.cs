using System.Windows;

namespace WpfApp1
{
    /// <summary>
    /// MainWindow.xaml 的交互逻辑
    /// </summary>
    public partial class MainWindow : Window
    {
        private int curPlayerCount;

        private string GetCurPlayer() => curPlayerCount++ % 2 == 0 ? "○" : "×";

        public MainWindow()
        {
            InitializeComponent();
            Reset();
        }

        private void Reset()
        {
            curPlayerCount = 1;
            Button11.Content = "";
            Button21.Content = "";
            Button31.Content = "";
            Button12.Content = "";
            Button22.Content = "";
            Button32.Content = "";
            Button13.Content = "";
            Button23.Content = "";
            Button33.Content = "";
        }

        private string SubJudge()
        {
            if ((string) Button11.Content != "" &&
                Button11.Content == Button21.Content &&
                Button21.Content == Button31.Content)
                return (string) Button11.Content;

            if ((string) Button12.Content != "" &&
                Button12.Content == Button22.Content &&
                Button22.Content == Button32.Content)
                return (string) Button12.Content;

            if ((string) Button13.Content != "" &&
                Button13.Content == Button23.Content &&
                Button23.Content == Button33.Content)
                return (string) Button13.Content;

            if ((string) Button11.Content != "" &&
                Button11.Content == Button12.Content &&
                Button12.Content == Button13.Content)
                return (string) Button11.Content;

            if ((string) Button21.Content != "" &&
                Button21.Content == Button22.Content &&
                Button22.Content == Button23.Content)
                return (string) Button21.Content;

            if ((string) Button31.Content != "" &&
                Button31.Content == Button32.Content &&
                Button32.Content == Button33.Content)
                return (string) Button31.Content;

            if ((string) Button11.Content != "" &&
                Button11.Content == Button22.Content &&
                Button22.Content == Button33.Content)
                return (string) Button11.Content;

            if ((string) Button13.Content != "" &&
                Button13.Content == Button22.Content &&
                Button22.Content == Button31.Content)
                return (string) Button13.Content;

            if ((string) Button11.Content != "" &&
                (string) Button21.Content != "" &&
                (string) Button31.Content != "" &&
                (string) Button12.Content != "" &&
                (string) Button22.Content != "" &&
                (string) Button32.Content != "" &&
                (string) Button13.Content != "" &&
                (string) Button23.Content != "" &&
                (string) Button33.Content != "")
                return "DogFall!";

            return "";
        }

        private void Judge()
        {
            var res = SubJudge();
            if (res == "") return;
            MessageBox.Show(res == "DogFall!" ? res : $"{res} Win!");
            if (MessageBox.Show("Reset?", "", MessageBoxButton.YesNo) == MessageBoxResult.No)
                Application.Current.Shutdown();
            Reset();
        }

        private void Button11_Click(object sender, RoutedEventArgs e)
        {
            if ((string) Button11.Content != "") return;
            Button11.Content = GetCurPlayer();
            Judge();
        }

        private void Button21_Click(object sender, RoutedEventArgs e)
        {
            if ((string) Button21.Content != "") return;
            Button21.Content = GetCurPlayer();
            Judge();
        }

        private void Button31_Click(object sender, RoutedEventArgs e)
        {
            if ((string) Button31.Content != "") return;
            Button31.Content = GetCurPlayer();
            Judge();
        }

        private void Button12_Click(object sender, RoutedEventArgs e)
        {
            if ((string) Button12.Content != "") return;
            Button12.Content = GetCurPlayer();
            Judge();
        }

        private void Button22_Click(object sender, RoutedEventArgs e)
        {
            if ((string) Button22.Content != "") return;
            Button22.Content = GetCurPlayer();
            Judge();
        }

        private void Button32_Click(object sender, RoutedEventArgs e)
        {
            if ((string) Button32.Content != "") return;
            Button32.Content = GetCurPlayer();
            Judge();
        }

        private void Button13_Click(object sender, RoutedEventArgs e)
        {
            if ((string) Button13.Content != "") return;
            Button13.Content = GetCurPlayer();
            Judge();
        }

        private void Button23_Click(object sender, RoutedEventArgs e)
        {
            if ((string) Button23.Content != "") return;
            Button23.Content = GetCurPlayer();
            Judge();
        }

        private void Button33_Click(object sender, RoutedEventArgs e)
        {
            if ((string) Button33.Content != "") return;
            Button33.Content = GetCurPlayer();
            Judge();
        }
    }
}
