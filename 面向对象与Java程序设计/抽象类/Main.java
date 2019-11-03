abstract class Shape
{
    abstract double Area();
}

class Triangle extends Shape
{
    public double a;
    public double b;
    public double c;
    public Triangle(double a, double b,double c)
    {
        this.a=a;
        this.b=b;
        this.c=c;
    }
    public double Area()
    {
        return Math.sqrt( ((a + b + c) / 2) * (((a + b + c) / 2) - a) * (((a + b + c) / 2) - b) * (((a + b + c) / 2) - c));
    }
}

class Rectangle extends Shape
{
    public double a;
    public double b;
    public Rectangle(double a, double b)
    {
        this.a=a;
        this.b=b;
    }
    public double Area()
    {
        return a * b;
    }
}

class Circle extends Shape
{
    public double r;
    public Circle(double r)
    {
        this.r=r;
    }
    public double Area()
    {
        return Math.PI * Math.pow(r, 2);
    }
}

public class Main 
{ 
    public static void main(String[] args) 
    {
        System.out.println(new Triangle(3, 4, 5).Area());
        System.out.println(new Rectangle(1, 1).Area());
        System.out.println(new Circle(1).Area());
    }
}
