class Rectangle
{
    private double width;
    private double height;
    private String color;
	/**
	 * @return the width
	 */
    public double getWidth() 
    {
		return width;
	}
	/**
	 * @param width the width to set
	 */
    public void setWidth(double width) 
    {
		this.width = width;
	}
	/**
	 * @return the height
	 */
    public double getHeight() 
    {
		return height;
	}
	/**
	 * @param height the height to set
	 */
    public void setHeight(double height) 
    {
		this.height = height;
	}
	/**
	 * @return the color
	 */
    public String getColor() 
    {
		return color;
	}
	/**
	 * @param color the color to set
	 */
    public void setColor(String color)
     {
		this.color = color;
	}
    public Rectangle(double width,double height,String color)
    {
        this.width = width;
        this.height = height;
        this.color = color;
    }
    public double getArea()
    {
        return width*height;
    }
    public double getLength()
    {
        return 2 * (width + height);
    }
}

public class Main 
{
    public static void main(String[] args) 
    {
        Rectangle r = new Rectangle(1, 2, "black");
        r.setColor("red");
        System.out.println(r.getColor());
        r.setHeight(4);
        System.out.println(r.getHeight());
        r.setWidth(4);
        System.out.println(r.getWidth());
        System.out.println(r.getArea());
        System.out.println(r.getLength());
    }
}
