import java.util.Date;
import java.util.stream.IntStream;

class Account
{
    private String id;
    private String name;
    private String date;
    private String password;
    private double bankBalance;
	/**
	 * @return the id
	 */
    public String getId() 
    {
		return id;
	}
	/**
	 * @return the name
	 */
    public String getName() 
    {
		return name;
	}
	/**
	 * @return the date
	 */
    public String getDate() 
    {
		return date;
	}
	/**
	 * @param password the password to set
	 */
    public void setPassword(String password) 
    {
		this.password = password;
	}
	/**
	 * @return the bankBalance
	 */
    public double getBankBalance() 
    {
		return bankBalance;
	}
    public Account(String name, double money)
    {
        this.name = name;
        bankBalance = money;
        id = IntStream.range(0, 11).map(x -> (int)(48 + Math.random() * 43)).boxed().map(x -> (char)(x < 58 || x > 64 ? x : x-7) + "").reduce((a, b) -> a + b).get();
        password="123456";
        date=new Date().toString();
    }
    public void Deposit(double money)
    {
        bankBalance+=money;
    }
    public void Draw(double money)
    {
        bankBalance-=money;
    }
}

public class Main 
{
    public static void main(String[] args) 
    {
        Account a = new Account("王成浩",9);
        a.Deposit(90.9);
        a.Draw(0.9);
        a.setPassword("654321");
        System.out.println(a.getId());
        System.out.println(a.getName());
        System.out.println(a.getDate());
        System.out.println(a.getBankBalance());
    }
}
