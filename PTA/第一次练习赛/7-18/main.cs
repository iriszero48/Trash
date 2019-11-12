using System;
using System.Collections.Generic;
public class Student
    {
        private string id;
        private string name;
        private double a;
        private double b;
        private double c;
        public Student(string input)
        {
            var r = input.Split();
            Init(r[0], r[1], r[2], r[3], r[4]);
        }
        private void Init(string id, string name, string a, string b, string c)
        {
            this.id = id;
            this.name = name;
            this.a = Convert.ToDouble(a);
            this.b = Convert.ToDouble(b);
            this.c = Convert.ToDouble(c);
        }
        public void Output()
        {
            Console.WriteLine(id.PadLeft(5) + name.PadLeft(5) + " " + String.Format("{0:F1}",(a + b + c) / 3));
        }
    }
    public class Program
    {
        public static void Main()
        {
            List<Student> students = new List<Student>();
            students.Add(new Student(Console.ReadLine()));
            students.Add(new Student(Console.ReadLine()));
            students.Add(new Student(Console.ReadLine()));
            students.Add(new Student(Console.ReadLine()));
            students.Add(new Student(Console.ReadLine()));
            foreach(var s in students)
            {
                s.Output();
            }
            Console.Read();
        }
    }
