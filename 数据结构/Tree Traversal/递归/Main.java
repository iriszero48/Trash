import java.util.function.Consumer;

class Node<T>
{
    private T Value;
    private Node<T> Left = null;
    private Node<T> Right =null;
    public Node(T v)
    {
        Value=v;
    }
    public Node(T v,Node<T> l,Node<T> r)
    {
        Value=v;
        Left=l;
        Right=r;
    }
    public void Preorder(Consumer<T> f)
    {
        f.accept(Value);
        if(Left != null) Left.Preorder(f);
        if(Right != null) Right.Preorder(f);
    }
    public void Inorder(Consumer<T> f)
    {
        if(Left != null) Left.Inorder(f);
        f.accept(Value);
        if(Right != null) Right.Inorder(f);
    }
    public void Postorder(Consumer<T> f)
    {
        if(Left != null) Left.Postorder(f);
        if(Right != null) Right.Postorder(f);
        f.accept(Value);
    }
}

public class Main 
{ 
    public static void main(String[] args) 
    {
        Node<Integer> t = new Node<Integer>(1, 
            new Node<Integer>(2, 
                new Node<Integer>(4, 
                    new Node<Integer>(7),null), 
                new Node<Integer>(5)), 
            new Node<Integer>(3, 
                new Node<Integer>(6, 
                    new Node<Integer>(8), 
                    new Node<Integer>(9)),
                null));
        Consumer<Integer> f = (x) -> System.out.print(x+" ");
        t.Preorder(f);
        System.out.println();
        t.Inorder(f);
        System.out.println();
        t.Postorder(f);
	}
}
