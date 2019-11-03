import java.util.Scanner;

class Main 
{
    public static <T extends Comparable<? super T>> void BubbleSort(T[] comparable) 
    {
        int count = comparable.length - 1;
        for(boolean swapped = true; swapped; --count)
        {
            swapped = false;
            for(int i = 0; i < count; ++i)
            {
                if (comparable[i].compareTo(comparable[i + 1]) > 0)
                {
                        T temp = comparable[i + 1];
                        comparable[i + 1] = comparable[i];
                        comparable[i] = temp;
                        swapped = true;
                }
            }
        }
    }

    public static void main(String[] args) 
    {
        Scanner scan = new Scanner(System.in);
        String[] input = scan.nextLine().split("\\s+");
        scan.close();
        Integer[] lst = new Integer[input.length];
        for(int i = 0; i < input.length; ++i)
        {
            lst[i] = Integer.parseInt(input[i]);
        }
        BubbleSort(lst);
        for(Integer i : lst)
        {
            System.out.print(i + " ");
        }
    }
}
