using System;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Imaging;
using System.Windows;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using static System.Math;
using Brushes = System.Drawing.Brushes;
using Image = System.Windows.Controls.Image;
using Rectangle = System.Drawing.Rectangle;

namespace ConsoleApp1
{
    public static class Program
    {
        private static readonly double G = (1 + Sqrt(5)) / 2;
        private const double T = 36 * (PI * 1) / 180;

        private static List<Tile> tiles = new List<Tile>();

        private const int w = 3840;
        private const int h = 2160;

        private enum Type
        {
            Kite,
            Dart
        }

        private struct Tile
        {
            public readonly double x;
            public readonly double y;
            public readonly double angle;
            public readonly double size;
            public readonly Type type;

            public Tile(Type t, double x, double y, double a, double s)
            {
                type = t;
                this.x = x;
                this.y = y;
                angle = a;
                size = s;
            }
        }

        private static BitmapSource Convert(Bitmap bitmap)
        {
            var bitmapData = bitmap.LockBits(
                new Rectangle(0, 0, bitmap.Width, bitmap.Height),
                ImageLockMode.ReadOnly, bitmap.PixelFormat);
            var bitmapSource = BitmapSource.Create(
                bitmapData.Width, bitmapData.Height,
                bitmap.HorizontalResolution, bitmap.VerticalResolution,
                PixelFormats.Bgr32, null,
                bitmapData.Scan0, bitmapData.Stride * bitmapData.Height, bitmapData.Stride);
            bitmap.UnlockBits(bitmapData);
            return bitmapSource;
        }

        private static List<Tile> SetupPrototiles(int w, int h)
        {
            var proto = new List<Tile>();
            for (var a = PI / 2 + T; a < 3 * PI; a += 2 * T)
                proto.Add(new Tile(Type.Kite, w / 2f, h / 2f, a, w / 2.5));
            return proto;
        }

        private static List<Tile> DeflateTiles(List<Tile> tls, int generation)
        {
            while (true)
            {
                if (generation <= 0) return tls;
                var next = new List<Tile>();
                foreach (var tile in tls)
                {
                    double x = tile.x, y = tile.y, a = tile.angle, nx, ny;
                    var size = tile.size / G;

                    if (tile.type == Type.Dart)
                    {
                        next.Add(new Tile(Type.Kite, x, y, a + 5 * T, size));
                        for (int i = 0, sign = 1; i < 2; i++, sign *= -1)
                        {
                            nx = x + Cos(a - 4 * T * sign) * G * tile.size;
                            ny = y - Sin(a - 4 * T * sign) * G * tile.size;
                            next.Add(new Tile(Type.Dart, nx, ny, a - 4 * T * sign, size));
                        }
                    }
                    else
                    {
                        for (int i = 0, sign = 1; i < 2; i++, sign *= -1)
                        {
                            next.Add(new Tile(Type.Dart, x, y, a - 4 * T * sign, size));
                            nx = x + Cos(a - T * sign) * G * tile.size;
                            ny = y - Sin(a - T * sign) * G * tile.size;
                            next.Add(new Tile(Type.Kite, nx, ny, a + 3 * T * sign, size));
                        }
                    }
                }
                tls = next;
                generation -= 1;
            }
        }


        [STAThread]
        private static void Main(string[] args)
        {
            var bmp = new Bitmap(w, h);
            tiles = DeflateTiles(SetupPrototiles(w, h), 5);
            double[,] dist = {{G, G, G}, {-G, -1, -G}};
            using (var g = Graphics.FromImage(bmp))
            {
                foreach (var tile in tiles)
                {
                    var angle = tile.angle - T;
                    var points = new List<PointF> {new PointF((float) tile.x, (float) tile.y)};
                    var ord = (int) tile.type;
                    for (var i = 0; i < 3; i++)
                    {
                        var x = tile.x + dist[ord, i] * Max(tile.size, 1f) * Cos(angle);
                        var y = tile.y - dist[ord, i] * Max(tile.size, 1f) * Sin(angle);
                        points.Add(new PointF((float) x, (float) y));
                        angle += T;
                    }
                    points.Add(new PointF((float) tile.x, (float) tile.y));
                    var arr = points.ToArray();
                    g.FillPolygon(ord == 0 ? Brushes.Orange : Brushes.Yellow, arr);
                    g.DrawPolygon(Pens.DarkGray, arr);
                }
            }
            //bmp.Save(@"z:\test.bmp");
            var image = new Image {Stretch = Stretch.Uniform, Source = Convert(bmp)};
            var windows = new Window {Content = image, Title = "Penrose tiling"};
            new Application().Run(windows);
        }
    }
}
