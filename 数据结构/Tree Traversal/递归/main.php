<?php
class Node
{
    private $Value;
    private $Left;
    private $Right;
    function __construct($v,$l = null,$r = null)
    {
        $this->Value=$v;
        $this->Left=$l;
        $this->Right=$r;
    }
    public function Preorder($f)
    {
        $f($this->Value);
        if($this->Left != null) $this->Left->Preorder($f);
        if($this->Right != null) $this->Right->Preorder($f);
    }
    public function Inorder($f)
    {
        if($this->Left != null) $this->Left->Inorder($f);
        $f($this->Value);
        if($this->Right != null) $this->Right->Inorder($f);
    }
    public function Postorder($f)
    {
        if($this->Left != null) $this->Left->Postorder($f);
        if($this->Right != null) $this->Right->Postorder($f);
        $f($this->Value);
    }
}
$tree = new Node(1,
    new Node(2,
        new Node(4,
            new Node(7)),
        new Node(5)),
    new Node(3,
        new Node(6,
            new Node(8),
            new Node(9))));
$f=function($x){echo $x;};
$tree->Preorder($f);
echo " ";
$tree->Inorder($f);
echo " ";
$tree->Postorder($f);
?>
