import java.util.*;

class Circle
{
    private double r;
    public Circle()
    {
        r = 0;
    }
    public Circle(double r)
    {
        this.r = r;
    }
    public double getRadius()
    {
        return r;
    }
    public double getPerimeter()
    {
        return 2 * Math.PI * r;
    }
    public void disp()
    {
        System.out.println(r);
        System.out.println(getPerimeter());
        System.out.println(Math.PI * r * r);
    }
}

class Cylinder extends Circle
{
    private double h;
    public Cylinder(double r, double h)
    {
        super(r);
        this.h = h;
    }
    public double getHeight()
    {
        return h;
    }
    public double getVol()
    {
        return Math.PI * getRadius() * getRadius() * h;
    }
    public void dispVol()
    {
        System.out.println(getVol());
    }
}

public class Main
{
    public static void main(String[] args) 
    {
        new Cylinder(new Scanner(System.in).nextDouble(), new Scanner(System.in).nextDouble()).dispVol();
    }
}
