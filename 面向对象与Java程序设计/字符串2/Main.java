public class Main 
{
    public static void main(String[] args) 
    {
        String s = "ddejidsEFALDFfnef2357 3ed";
        System.out.println(s.split("[A-Z]").length - 1);
        System.out.println(s.split("[a-z]").length - 1);
        System.out.println(s.split("[^A-Za-z]").length - 1);
    }
}
