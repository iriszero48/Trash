using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static System.Console;
using static System.Convert;

namespace GPA_CUI_Calculator
{
    public class Subject
    {
        public string subject;
        public double Credit;
        private double garde;
        public double Grade
        {
            set
            {
                garde = value < 60 ? 0 : value / 10 - 5;
            }
            get
            {
                return garde;
            }
        }
        public Subject(string subject,double credit)
        {
            this.subject = subject;
            Credit = credit;
        }
    }
    class Program
    {
        static string GetSubject()
        {
            Write("科目：");
            return ReadLine();
        }
        static double GetCredit()
        {
            Write("学分：");
            return ToDouble(ReadLine());
        }
        static void Main(string[] args)
        {
            var subjects = new List<Subject>();
            for (string input = GetSubject(); input != ""; input = GetSubject())
            {
                subjects.Add(new Subject(input, GetCredit()));
            }
            Clear();
            while (true)
            {
                for(var i = 0;i<subjects.Count;++i)
                {
                    Write(subjects[i].subject + "：");
                    subjects[i].Grade = ToDouble(ReadLine());
                }
                WriteLine(subjects.Sum(i => i.Grade * i.Credit) / subjects.Sum(i => i.Credit));
                ReadLine();
            }
        }
    }
}
