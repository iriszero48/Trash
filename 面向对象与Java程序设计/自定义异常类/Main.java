class DangerException extends Exception
{
    public void toshow()
    {
        System.out.println("危险品");
    }
}

class Goods
{
    private boolean isDanger;

	/**
	 * @return the isDanger
	 */
    public boolean getIsDanger() 
    {
		return isDanger;
	}

	/**
	 * @param isDanger the isDanger to set
	 */
    public void setIsDanger(boolean isDanger) 
    {
		this.isDanger = isDanger;
    }
    public Goods(boolean danger)
    {
        isDanger = danger;
    }
}

class Machine
{
    public void checkBag(Goods goods) throws DangerException
    {
        if(goods.getIsDanger()) throw new DangerException();
    }
}

public class Main
{
    public static void main(String[] args) 
    {
        Goods goods = new Goods(true);
        try
        {
            new Machine().checkBag(goods);
        }
        catch(DangerException exception)
        {
            exception.toshow();
        }
    }
}
