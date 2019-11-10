using System;
using System.Windows;
using System.Windows.Input;

namespace bacwpf
{
    /// <summary>
    /// MainWindow.xaml 的交互逻辑
    /// </summary>
    public partial class MainWindow : Window
    {
        private string guess;
        private int times;
        private string result;
        public MainWindow() => InitializeComponent();
        private bool IsRepeat()
        {
            for (int i = 0; i < 4; ++i)
            {
                for (int j = 0; j < 4; ++j)
                {
                    if (guess[i] == guess[j] && i != j)
                    {
                        return true;
                    }
                }
            }
            return false;
        }
        private bool Jugde()
        {
            int m = 0, n = 0;
            string input = Input.Text;
            for (int i = 0; i < 4; ++i)
            {
                if (input[i] == guess[i])
                {
                    ++m;
                }
                for (int j = 0; j < 4; ++j)
                {
                    if (input[i] == guess[j] && j != i)
                    {
                        ++n;
                    }
                }
            }
            result = m + "A" + n + "B";
            ++times;
            if (m == 4)
            {
                return true;
            }
            return false;
        }
        private void Start_MouseLeftButtonUp(object sender, MouseButtonEventArgs e)
        {
            do
            {
                guess = Convert.ToString(new System.Random().Next(1000, 10000));
            } while (IsRepeat() || guess[0] == '0');
            times = 0;
            result = "";
            Start.Visibility = Visibility.Hidden;
        }
        private void Relus_MouseLeftButtonUp(object sender, MouseButtonEventArgs e) => Relus.Visibility = Visibility.Hidden;

        private void Button_Click(object sender, RoutedEventArgs e)
        {
            if(Jugde())
            {
                MessageBoxResult win = MessageBox.Show($"你太棒了，这个数字就是{guess}，你一共猜了{times}次哦", "恭喜", MessageBoxButton.OK, MessageBoxImage.Information);
                if (win == MessageBoxResult.OK) 
                {
                    Environment.Exit(0);
                }
            }
            else
            {
                MessageBox.Show(result, "提示", MessageBoxButton.OK, MessageBoxImage.Information);
                if(times==7)
                {
                    MessageBoxResult rest = MessageBox.Show("你已经猜了 8 次了，还要继续吗？", "提示", MessageBoxButton.YesNo, MessageBoxImage.Information);
                    if(rest==MessageBoxResult.No)
                    {
                        Environment.Exit(0);
                    }
                }
                if(times==15)
                {
                    MessageBoxResult gameover = MessageBox.Show("你已经猜了 15 次啦，是不是方法不对啊？ 休息一会再来玩吧！", "提示", MessageBoxButton.OK, MessageBoxImage.Information);
                    if(gameover==MessageBoxResult.OK)
                    {
                        Environment.Exit(0);
                    }
                }
            }
        }
    }
}
