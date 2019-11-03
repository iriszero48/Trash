node() 
{
  value[$1]=$1
  left[$1]=$2
  right[$1]=$3
}

node 1 2 3
node 2 4 5
node 3 6
node 4 7
node 5
node 6 8 9
node 7
node 8
node 9
 
traversal() 
{
  local nx=$1
  shift
  for branch in $@ ; do
    case "$branch" in
      left) if [ ${left[$nx]} ]; then traversal ${left[$nx]} $@ ; fi ;;
      right) if [ "${right[$nx]}" ]; then traversal ${right[$nx]} $@ ; fi ;;
      self) printf "%d " ${value[$nx]} ;;
    esac
  done
}
 
preorder() { traversal $1 self left right ; }
inorder()   { traversal $1 left self right ; }
postorder() { traversal $1 left right self ; }

preorder 1
echo ''
inorder 1
echo ''
postorder 1
