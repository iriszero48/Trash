using System;
using System.IO;
using System.Linq;
using System.Text;

namespace ConsoleApp1
{
    internal static class Program
    {
        private static string ConvertToDigit(string args)
        {
            string[] wrongStr = { "○", "一", "二", "三", "四", "五", "六", "七", "八", "九", "十", "千", "百", "正" };
            string[] standard = { "零", "壹", "贰", "叁", "肆", "伍", "陆", "柒", "捌", "玖", "拾", "仟", "佰", "整" };
            string[] rmb = { "兆", "仟", "佰", "拾", "亿", "仟", "佰", "拾", "万", "仟", "佰", "拾", "元", "角", "分" };
            string[] num = { "0", "1", "2", "3", "4", "5", "6", "7", "8", "9" };
            for (var i = 0; i < wrongStr.Length; i++) args = args.Replace(wrongStr[i], standard[i]);
            for (var i = 0; i < num.Length; i++) args = args.Replace(standard[i], num[i]);
            args = args.Replace("0", "").Replace("整", "");
            if (args.IndexOf("元", StringComparison.Ordinal) < 0) args = "0元" + args;
            var str = "";
            for (int i = args.Length - 1, idx = rmb.Length; i > 0; i--)
            {
                var tmp = args.Substring(i, 1);
                if (char.IsNumber(tmp, 0)) continue;
                while (true)
                {
                    idx -= 1;
                    if (tmp == rmb[idx])
                    {
                        var t = args.Substring(i - 1, 1);
                        str = (char.IsNumber(t, 0) ? t : "0") + tmp + str;
                        break;
                    }
                    str = "0" + rmb[idx] + str;
                }
            }
            str = str.Replace("元", ".");
            if (args[0].Equals('拾')) str = "1" + str;
            return rmb.Aggregate(str, (current, t) => current.Replace(t, ""));
        }

        private static void Main(string[] args)
        {
            Console.WriteLine(
                File.ReadAllLines(@"z:\bills.csv", Encoding.Default)
                .Skip(1)
                .Select(x => x.Split(','))
                .Select(x => Convert.ToDecimal(ConvertToDigit(x[0])) * Convert.ToInt32(x[1])).Sum());
        }
    }
}
