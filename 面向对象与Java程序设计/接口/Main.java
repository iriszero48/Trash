interface Shape
{
    double size();
}

class Line implements Shape
{
    public double l;
    public Line(double l)
    {
        this.l = l;
    }
    public double size()
    {
        return l;
    }
}

class Circle implements Shape
{
    public double r;
    public Circle(double r)
    {
        this.r=r;
    }
    public double size()
    {
        return 2 * Math.PI * r;
    }
}

public class Main 
{ 
    public static void main(String[] args) 
    {
        System.out.println(new Line(1).size());
        System.out.println(new Circle(1).size());
    }
}
