using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;
using System.Web.UI.DataVisualization.Charting;

namespace CalculatorVerII
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Now Loading...");
            List<Element> elements = new List<Element>();
            Calculate calculate = new Calculate();
            Console.WriteLine("Calculator Ver II");
            while (calculate.calcilate())
                Console.WriteLine("[done]");
        }
    }
    static public class F
    {
        public const string r = "real";
        public const string c = "complex";
        public const string l = "list";
        public const string v = "vector";
    }
    static public class Error
    {
        public const string IS = "invalid syntax";
    }










    [Serializable]
    public sealed class Element
    {
        public string Flag { get; set; }
        public string Symbol;
        public double real;
        public Complex complex;
        public Quaternions quaternions;
        public Vector vector;
        public List list;
        public RealMatrix realMatrix;
        public RealVector realVector;
        public ComplexMatrix complexMatrix;
        public ComplexVector complexVector;
        public void IInitialize()
        {

        }
        public void Clear()
        {
            Flag = null;
            real = 0;
            
        }
        public void ToReal()
        {

        }
        public void ToList()
        {
            list = new List();
            list.list = new List<double>();
            list.list.Add(real);
            real = 0;
            Flag = F.l;
        }
        private void ToComplex(string origin)
        {

        }
        private void ToQuaternions(string origin)
        {

        }
        private void ToVector(string origin,byte comma)
        {

        }
        private void ToList(string origin)
        {
            char[] comma = { ',' };
            list = new List();
            list.list = new List<double>();
            foreach (string number in origin.Split(comma))
                list.list.Add(Convert.ToDouble(number));
            Flag = F.l;
        }
        private void ToRealMatrix(string origin)
        {

        }
        private void ToRealVector(string origin)
        {

        }
        private void ToComplexMatrix(string origin)
        {

        }
        private void ToComplexVector(string origin)
        {

        }
        /*
         * real
         * complex          1,2                     
         * v                v1,2,3                      1v+2, or 1v+1,
         * list             l1,2,3,4,5               
         * qu               q1,2,3,4                    
         * rv               rv1,2,3,4,5,6               
         * rm               rm1,2,3;1,2,3               
         * cv               cv1,2;
         * cm               cm1,2'3,4;
         */



        public Element(double input)
        {
            real = input;
            Flag = F.r;
        }
        public Element(string raw)
        {
            byte comma = 0;
            byte semicolon = 0;
            byte singleQuotationMark = 0;
            bool v = false;
            bool l = false;
            bool q = false;
            bool r = false;
            bool m = false;
            bool c = false;
            if (raw == "")
                raw = "ans";
            char[] rawChars = raw.ToCharArray();
            switch (rawChars[0])
            {
                case 'v':
                    v = true;
                    raw = raw.Remove(0, 1);
                    break;
                case 'l':
                    l = true;
                    raw = raw.Remove(0, 1);
                    break;
                case 'q':
                    q = true;
                    raw = raw.Remove(0, 1);
                    break;
                case 'r':
                    r = true;
                    raw = raw.Remove(0, 1);
                    break;
                case 'c':
                    c = true;
                    raw = raw.Remove(0, 1);
                    break;
            }
            if (rawChars.Length > 1)
            {
                switch (rawChars[1])
                {
                    case 'v':
                        v = true;
                        raw = raw.Remove(0, 1);
                        break;
                    case 'm':
                        m = true;
                        raw = raw.Remove(0, 1);
                        break;
                }
            }
            foreach(char rawChar in rawChars)
                if (rawChar == ',')
                    ++comma;           
            if (v&&(comma==2||comma==3))
            {
                ToVector(raw, comma);
            }
            else if(l)
            {
                ToList(raw);
            }
            else if (comma == 1)
            {
                ToComplex(raw);
            }
            else if (double.TryParse(raw, out real))
            {
                Flag = F.r;
            }
            else
            {
                Symbol = raw;
            }
        }









         ///ver1.00
        /*public Element(string Origins)
        {
            bool Comma = false;
            bool Semicolon = false;
            bool SingleQuotationMarks = false;
            bool DoubleQuotationMarks = false;
            foreach (char origin in Origins.ToCharArray())
            {
                switch (origin)
                {
                    case ',':
                        Comma = true;
                        break;
                    case ';':
                        Semicolon = true;
                        break;
                    case '\'':
                        SingleQuotationMarks = true;
                        break;
                    case '\"':
                        DoubleQuotationMarks = true;
                        break;
                }
            }
            if (Semicolon)
            {
                if (DoubleQuotationMarks)
                {
                    if (SingleQuotationMarks)
                    {
                        ToComplexMatrix(Origins);
                    }
                    else
                    {
                        ToComplexVector(Origins);
                    }
                }
                else
                {
                    ToComplex(Origins);
                }
            }
            else

                if (SingleQuotationMarks)
                if (DoubleQuotationMarks)
                {
                    ToRealMatrix(Origins);
                }
                else
                {
                    ToList(Origins);
                }
            else
                    if (Comma)
                if (Semicolon)
                    ToQuaternions(Origins);
                else
                    ToVector(Origins);
            else
                        if (DoubleQuotationMarks)
            {
                ToRealVector(Origins);
            }
            else
                try
                {
                    real = Convert.ToDouble(Origins);
                    Flag = (F.r);
                    Symbol = null;
                }
                catch
                {
                    Symbol = Origins;
                }

        }*/
    }
    
    public sealed class Flag
    {
        private string flag;
        public string Get()
        {
            return flag;
        }
        public void Set(string newFlag)
        {
            flag = newFlag;
        }
    }
    [Serializable]
    public sealed class Complex
    {
        public double re;
        public double im;
        public double Abs()
        {
            return Math.Sqrt(re * re + im * im);
        }
        public double[] Conj()
        {
            double[] complex = new double[2];
            complex[0] = re;
            complex[1] = -im;
            return complex;
        }
    }
    public sealed class Quaternions
    {
        public double a;
        public double b;
        public double c;
        public double im;
    }
    public sealed class Vector
    {
        public List<double> vector;
        public double Abs()
        {
            double Total = 0;
            foreach (double sVector in vector)
                Total += Math.Pow(sVector, 2);
            return Math.Sqrt(Total);
        }
    }
    public sealed class List
    {
        public List<double> list;
        public List<double> DList()
        {
            List<double> Sub = new List<double>();
            for (int i = 1; i < list.Count; ++i)
                Sub.Add(list[i] - list[i - 1]);
            return Sub;
        }
        public double IIList()
        {
            double Total = 1;
            foreach (Double sList in list)
                Total *= sList;
            return Total;
        }
        public double Pos(int pos)
        {
            return list[pos - 1];
        }
        public List<double> Peverse()
        {
            List<double> Sub = new List<double>();
            for (int pos = list.Count - 1; pos >= 0; --pos)
                Sub.Add(list[pos]);
            return Sub;
        }
        public double Size()
        {
            return list.Count;
        }
        public double SumList()
        {
            double Total = 0;
            foreach (double sList in list)
                Total += sList;
            return Total;
        }
    }
    public sealed class RealMatrix
    {
        public List<List> realMatrix;
    }
    public sealed class ComplexVector
    {
        public List<Complex> complexVector;
    }
    public sealed class RealVector
    {
        public List<double> realVector;
    }
    public sealed class ComplexMatrix
    {
        public List<ComplexVector> complexMatrix;
    }
    /*public interface IComplexDBvIII
    {
        Complex DBvIII();
        void DBvIII(double re, double im);
    }
    public interface IRealDBvIII:IComplexDBvIII
    {
        double DBvIII();
        void DBvIII(double re);
    }*/










    public class ComplexVar
    {
        public void DBvIII(string ID, double re, double im)
        {
            Complex complex = new Complex();
            complex.re = re;
            complex.im = im;
            IFormatter DBvIII = new BinaryFormatter();
            FileStream Initialize = new FileStream(ID + ".DBvIII", FileMode.OpenOrCreate, FileAccess.Write);
            DBvIII.Serialize(Initialize, complex);
            Initialize.Close();
        }
        public Complex DBvIII(string ID, bool none)
        {
            try
            {
                FileStream Initialize = new FileStream(ID + ".DBvIII", FileMode.Open, FileAccess.Read);
                IFormatter DBvIII = new BinaryFormatter();
                Complex complex = DBvIII.Deserialize(Initialize) as Complex;
                Initialize.Close();
                return complex;
            }
            catch
            {
                DBvIII(ID, 0, 0);
                Complex complex = new Complex();
                complex.re = 0;
                complex.im = 0;
                return complex;
            }
        }
    }
    public class NumberFormat : ComplexVar
    {

    }
    public class RealVar : ComplexVar
    {
        public void DBvIII(string ID, double re)
        {
            DBvIII(ID, re, 0);
        }
        public double DBvIII(string ID)
        {
            return DBvIII(ID, true).re;
        }
    }
    public class AngleMeasure : RealVar
    {

    }
    public class Ans:ComplexVar
    {
        public void DBvIII(Element ans)
        {
            Complex complex = new Complex();
            IFormatter DBvIII = new BinaryFormatter();
            FileStream Initialize = new FileStream("ans.DBvIII", FileMode.OpenOrCreate, FileAccess.Write);
            DBvIII.Serialize(Initialize, complex);
            Initialize.Close();
        }
        public Complex DBvIII(string ID, bool none)
        {
            try
            {
                FileStream Initialize = new FileStream("ans.DBvIII", FileMode.Open, FileAccess.Read);
                IFormatter DBvIII = new BinaryFormatter();
                Complex complex = DBvIII.Deserialize(Initialize) as Complex;
                Initialize.Close();
                return complex;
            }
            catch
            {
                DBvIII(ID, 0, 0);
                Complex complex = new Complex();
                complex.re = 0;
                complex.im = 0;
                return complex;
            }
        }
    }






    /*
     * dbvi         ascii
     * dbvii        compress
     * dbviii       binary
     * dbviv        7zip
     * dbvv         xml
     */









    public class Basic
    {
        public List<Element> elements = new List<Element>();
        public void Delete(int pos,int times)
        {
            for (int t = 0; t < times; ++t)
                elements.RemoveAt(pos);
        }
        public void Replace(Element x, int pos)
        {
            switch (x.Flag)
            {
                case F.r:
                    
                    elements[pos].Flag = F.r;
                    elements[pos].real = x.real;
                    break;
                case F.l:
                    elements[pos].Flag = F.l;
                    break;
            }
        }
    }
    public class Sixth : Basic
    {
        public void sixth()
        {

        }
    }
    public class Fifth : Sixth
    {
        public void fifth()
        {
            for (int point = 0; point < elements.Count; ++point)
            {
                switch (elements[point].Symbol)
                {
                    case "*":
                        Replace(Mathematics.Multiplication(elements[point - 1], elements[point + 1]), point - 1);
                        Delete(point, 2);
                        break;
                }
            }
            for (int point = 0; point < elements.Count; ++point)
            {
                switch (elements[point].Symbol)
                {
                    case "+":
                        Replace(Mathematics.Plus(elements[point - 1], elements[point + 1]), point - 1);
                        Delete(point, 2);
                        break;
                }
            }
        }
    }
    public class Fouth : Fifth
    {
        public void fouth()
        {
            for (int point = 0; point < elements.Count; ++point)
            {
                switch (elements[point].Symbol)
                {
                    case "kbtogb":
                        Replace(Mathematics.Kbtogb(elements[point + 1]), point);
                        Delete(point + 1, 1);
                        break;
                    case "gamma":
                        Replace(Mathematics.Gamma(elements[point + 1]), point);
                        Delete(point + 1, 1);
                        break;
                    case "beta":
                        Replace(Mathematics.Beta(elements[point + 1], elements[point + 2]), point);
                        Delete(point + 1, 2);
                        break;
                }
            }
        }
    }
    public class Third : Fouth
    {
        public void third()
        {

        }
    }
    public class Second : Third
    {
        public void second()
        {

        }
    }
    public class First : Second
    {
        public void first()
        {

        }
    }
    public class Calculate : First
    {
        public bool calcilate()
        {
            string inputRaw = Console.ReadLine();
            char[] separator = { ' ' };
            string[] inputs = inputRaw.Split(separator);
            foreach (string input in inputs)
                elements.Add(addElement(input));
            first();
            second();
            third();
            fouth();
            fifth();
            sixth();
            output();
            elements.Clear();
            return true;
        }
        private Element addElement(string input)
        {
            Element newElement = new Element(input);
            return newElement;
        }
        private void output()
        {
            switch(elements[0].Flag)
            {
                case F.r:
                    Console.WriteLine(elements[0].real);
                    break;
            }            
        }      
    }











    public static class Mathematics
    {
        public static bool isList(ref Element x)
        {
            if (x.Flag == F.c)
                if (x.complex.im == 0)
                    x.ToReal();
            if (x.Flag == F.r)
            {
                x.ToList();
                return true;
            }
            if (x.Flag == F.l)
                return false;
            else
                throw new Exception(Error.IS);
        }
        public static Element Plus(Element a,Element b)
        {
            switch (b.Flag)
            {
                case F.r:
                    a.real += b.real;
                    break;
            }
            return a;
        }
        public static Element Multiplication(Element a, Element b)
        {
            switch (b.Flag)
            {
                case F.r:
                    a.real *= b.real;
                    break;
            }
            return a;
        }
        public static Element Kbtogb(Element x)
        {
            bool real = isList(ref x);
            for (int pos = 0; pos < x.list.list.Count; ++pos)
                x.list.list[pos] = x.list.list[pos] / 1024 / 1024;
            if (real)
            {
                Element result = new Element(x.list.list[0]);
                return result;
            }
            return x;
        }
        public static Element Gamma (Element x)
        {
            bool real = isList(ref x);
            Chart chart = new Chart();
            for (int pos = 0; pos < x.list.list.Count; ++pos)
                x.list.list[pos] = chart.DataManipulator.Statistics.GammaFunction(x.list.list[pos]);
            if (real)
            {
                Element result = new Element(x.list.list[0]);
                return result;
            }
            return x;
        }
        public static Element Beta(Element x,Element y)
        {
            Chart chart = new Chart();
            x.real = chart.DataManipulator.Statistics.BetaFunction(x.real, y.real);
            return x;
        }
    }
}










/*
 * 1st  command  
 * 2nd  sp
 * 3rd  ()
 * 4th  function
 * 5th  base
 * 6th  bool
 */
