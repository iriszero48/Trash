using System.IO;
using System.Linq;

namespace MSV
{
    internal static class Program
    {
        private static void StuCopy(string s, string d, string kw)
        {
            var input = Directory.EnumerateFiles(s).Select(Path.GetFileName).First(x => x.StartsWith(kw));
            File.Copy(Path.Combine(s, input), Path.Combine(d, Path.GetFileName(s) + Path.GetExtension(input)));
        }

        private static void Main(string[] args)
        {
            const string passStr = "行程卡";
            const string hpStr = "健康码";
            var path = args[0];
            var all = Directory.GetDirectories(path);
            var pass = Directory.CreateDirectory(Path.Combine(path, passStr));
            var hp = Directory.CreateDirectory(Path.Combine(path, hpStr));
            foreach (var stu in all)
            {
                StuCopy(stu, pass.FullName, passStr);
                StuCopy(stu, hp.FullName, hpStr);
            }
        }
    }
}
