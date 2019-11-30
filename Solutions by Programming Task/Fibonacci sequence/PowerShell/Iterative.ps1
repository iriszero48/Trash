function fib($x)
{
    if($x -lt 2) { return $x }
    $p, $n = 0, 1
    0 .. ($x - 1) | % {$p, $n = $n, ($p + $n)}
    return $p
}
