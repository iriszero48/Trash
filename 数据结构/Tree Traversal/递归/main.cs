using System;
using System.Collections.Generic;

namespace Temp
{
    public class Node<T>
    {
        private T Value;
        private Node<T> Left;
        private Node<T> Right;
        public Node(T v, Node<T> l = default, Node<T> r = default)
        {
            Value = v;
            Left = l;
            Right = r;
        }
        public IEnumerable<T> Preorder()
        {
            yield return Value;
            if (Left != default) foreach (var v in Left.Preorder()) yield return v;
            if (Right != default) foreach (var v in Right.Preorder()) yield return v;
        }
        public IEnumerable<T> Inorder()
        {
            if (Left != default) foreach (var v in Left.Inorder()) yield return v;
            yield return Value;
            if (Right != default) foreach (var v in Right.Inorder()) yield return v;
        }
        public IEnumerable<T> Postorder()
        {
            if (Left != default) foreach (var v in Left.Postorder()) yield return v;
            if (Right != default) foreach (var v in Right.Postorder()) yield return v;
            yield return Value;
        }
    }
    class Temp
    {
        static void Main(string[] args)
        {
            var t = 
                new Node<int>(1, 
                    new Node<int>(2, 
                        new Node<int>(4, 
                            new Node<int>(7)), 
                        new Node<int>(5)), 
                    new Node<int>(3, 
                        new Node<int>(6, 
                            new Node<int>(8), 
                            new Node<int>(9))));
            foreach (var f in new Func<IEnumerable<int>>[] { t.Preorder, t.Inorder, t.Postorder })
            {
                Console.WriteLine("{0}:{1}", f.Method.Name, string.Join(" ", f()));
            }
        }
    }
}
