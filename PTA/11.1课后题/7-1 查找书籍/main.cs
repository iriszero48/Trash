using static System.Console;
using static System.Convert;
using System.Collections.Generic;
using System.Linq;
using System;
namespace TEMP_CS
{
    public class Book
    {
        private string name;
        private double price;
        public Book(string name,double price)
        {
            this.name = name;
            this.price = price;
        }
        public string Name { get => name;}
        public double Price { get => price;}
        public bool Display()
        {
            WriteLine($"{String.Format("{0:F}", Price)}, {Name}");
            return true;
        }
    }
    public class Program
    {
        public static void Main()
        {
            List<Book> books = new List<Book>();
            int count = ToInt32(ReadLine());
            for(int i=0;i<count;++i)
            {
                books.Add(new Book(ReadLine(), ToDouble(ReadLine())));
            }
            books.OrderBy(book => book.Price).Reverse().ToList()[0].Display();
            books.OrderBy(book => book.Price).ToList()[0].Display();
        }
    }
}
