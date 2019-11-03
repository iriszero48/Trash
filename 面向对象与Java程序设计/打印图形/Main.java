import java.util.Collections;
import java.util.stream.IntStream;

class Main 
{
    public static void Show()
    {
        IntStream.rangeClosed(1, 5).forEach(a -> System.out.println(String.join("", Collections.nCopies(5 - a, " ")) + String.join(" ", Collections.nCopies(a, "*"))));
    }
    public static void main(String[] args) 
    {
        Show();
    }
}
