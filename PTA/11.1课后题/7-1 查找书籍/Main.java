import java.util.*;
public class Main 
{
	public static void main(String[] args) 
	{
        Scanner inpu=new Scanner(System.in);
        int count = Integer.parseInt(inpu.nextLine());
        String maxn="",minn="";
        double minp=100000000,maxp=0;
        for(int i=0;i<count;++i)
        {
        	String inn=inpu.nextLine();
        	double inp=Double.parseDouble(inpu.nextLine());
        	if(inp>maxp)
        	{
        		maxp=inp;
        		maxn=inn;
        	}
        	if(inp<minp)
        	{
        		minp=inp;
        		minn=inn;
        	}
        }
        System.out.println(String.format("%.2f", maxp)+", "+maxn);
        System.out.println(String.format("%.2f", minp)+", "+minn);
	}
}
