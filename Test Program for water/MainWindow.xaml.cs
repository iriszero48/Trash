using System;
using System.Windows;
using System.Windows.Threading;

namespace WATER
{
    /// <summary>
    /// MainWindow.xaml 的交互逻辑
    /// </summary>
    public partial class MainWindow : Window
    {
        private DispatcherTimer timer;
        private DispatcherTimer timer2;
        int value = 100;
        int cm = 0;
        public MainWindow()
        {
            InitializeComponent();
            Loaded += new RoutedEventHandler(Window2_Loaded);
            Loaded += new RoutedEventHandler(Window1_Loaded);
        }       
        private void change(object sender, RoutedEventArgs e)
        {
            Random rd = new Random();
            value = rd.Next(0, 100);
            //textBox.Text = Convert.ToString(value);
            /*
            if (value != 100)
            {
                MessageBox.Show("Only 100cm can fuck it", "Error");
                value = 100;
                textBox.Text = Convert.ToString(value);
            }*/
        }
        void Window1_Loaded(object sender, RoutedEventArgs e)
        {
            timer = new DispatcherTimer();
            timer.Interval = TimeSpan.FromSeconds(3);
            timer.Tick += timer1_Tick;
            timer.Start();
        }
        void Window2_Loaded(object sender, RoutedEventArgs e)
        {
            timer = new DispatcherTimer();
            timer.Interval = TimeSpan.FromSeconds(1);
            timer.Tick += timer2_Tick;
            timer.Start();
        }

        private void timer1_Tick(object sender, EventArgs e)
        {
            if (value != 100)
            {
                ++cm;
                textBox2.Text = Convert.ToString(cm + " times");
                ///value = 100;
                //textBox.Text = Convert.ToString(value);
            }
        }
        private void timer2_Tick(object sender, EventArgs e)
        {
            textBox.Text = Convert.ToString(value + " cm");
        }
    }
}
