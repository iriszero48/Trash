using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
namespace Cue_Ver.I
{
    class CueBuild
    {
        static void Main(string[] args)
        {
            List<string> keyWords = new List<string>();
            const string date = "REM DATE";
            const string genre = "REM GENRE";
            const string performer = "PERFORMER";
            const string title = "TITLE";
            const string file = "FILE";
            const string wave = "WAVE";
            const string track = "TRACK";
            const string audio = "AUDIO";
            const string index = "INDEX 01";
            keyWords.Add(item: date);
            keyWords.Add(item: genre);
            keyWords.Add(item: performer);
            keyWords.Add(item: title);
            keyWords.Add(item: file);
            string Cue = "";
            Console.WriteLine(value: "Cue Ver.I");
            Console.Write(value: "Cue Path ");
            string path = Console.ReadLine();
            foreach(string keyWord in keyWords)
            {
                Console.Write(value: $"{path}>{keyWord} ");
                if (keyWord == performer || keyWord == title)
                {
                    Build(keyWord, b: Console.ReadLine(), Cue: ref Cue, bDoubleQuotationMarks: true);
                }
                else if (keyWord == file)
                {
                    Build(a: keyWord, b: Console.ReadLine(), c: wave, Cue: ref Cue, bDoubleQuotationMarks: true);
                }
                else if (keyWord == date)
                {
                    Build(a: keyWord, b: Date(), Cue: ref Cue);
                }
                else
                {
                    Build(keyWord, b: Console.ReadLine(), Cue: ref Cue);
                }
            }
            keyWords.Clear();
            keyWords.Add(item: title);
            keyWords.Add(item: performer);
            keyWords.Add(item: genre);
            keyWords.Add(item: index);
            Console.Write(value: $"{path}>track ");
            byte Track = Convert.ToByte(value: Console.ReadLine());
            for(byte tr=1;tr<=Track;++tr)
            {
                string trString = tr.ToString().PadLeft(totalWidth: 2, paddingChar: '0');
                Build(track, b: trString, c: audio, Cue: ref Cue, bDoubleQuotationMarks: false, Space: true);
                foreach(string keyWord in keyWords)
                {
                    Console.Write(value: $"{path}>{track} {trString} {keyWord} ");
                    if (keyWord == performer || keyWord == title)
                    {
                        Build(keyWord, b: Console.ReadLine(), Cue: ref Cue, bDoubleQuotationMarks: true, Space: null);
                    }
                    else if (keyWord == index)
                    {
                        Build(keyWord, b: Time(), Cue: ref Cue, bDoubleQuotationMarks: false, Space: null);
                    }
                    else
                    {
                        Build(keyWord, b: Console.ReadLine(), Cue: ref Cue, bDoubleQuotationMarks: false, Space: null);
                    }
                }
            }
            StreamWriter CueFile = new StreamWriter(path: path, append: false, encoding: Encoding.UTF8);
            CueFile.Write(value: Cue);
            CueFile.Close();
            Console.WriteLine(value: $"[done] {path}");
            Console.ReadLine();
        }
        public static void Build(string a, string b, ref string Cue, bool bDoubleQuotationMarks = false, bool? Space = false) => Build(a: a, b: b, c: null, Cue: ref Cue, bDoubleQuotationMarks: bDoubleQuotationMarks, Space: Space);
        public static void Build(string a,string b,string c,ref string Cue, bool bDoubleQuotationMarks=false, bool? Space = false)
        {
            string sub = "";
            if (Space == true)
            {
                sub = "  ";
            }

            if (Space == null)
            {
                sub = "    ";
            }

            sub += a;
            sub += " ";
            string sub2 = "";
            if (bDoubleQuotationMarks)
            {
                sub2 = "\"";
                sub2 += b;
                sub2 += "\"";
            }
            else
            {
                sub2 = b;
            }

            if (c!=null)
            {
                sub2 += " ";
                sub2 += c;
            }
            sub2 += "\n";
            sub += sub2;
            Cue += sub;
        }
        public static string Time()
        {
            string time = "";
            Console.WriteLine("");
            Console.Write(value: "hh ");
            time += Console.ReadLine().PadLeft(totalWidth: 2, paddingChar: '0');
            time += ":";
            Console.Write(value: "mm ");
            time += Console.ReadLine().PadLeft(totalWidth: 2, paddingChar: '0');
            time += ":";
            Console.Write(value: "ss ");
            time += Console.ReadLine().PadLeft(totalWidth: 2, paddingChar: '0');
            return time;
        }
        public static string Date()
        {
            string date = "";
            Console.WriteLine("");
            Console.Write(value: "yyyy ");
            date += Console.ReadLine();
            date += ".";
            Console.Write(value: "mm ");
            date += Console.ReadLine().PadLeft(totalWidth: 2, paddingChar: '0');
            date += ".";
            Console.Write(value: "dd ");
            date += Console.ReadLine().PadLeft(totalWidth: 2, paddingChar: '0');
            return date;
        }
    }
}
