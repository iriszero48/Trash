<?php

function f($n, $x, $y)
{
    return ($x + $y * 2 + 1) % $n;
}

if ($argc != 2) return 1;

$n = intval($argv[1]);
if ($n < 3 || ($n % 2) == 0) return 2;
foreach (range(0,$n) as $i)
{
    foreach (range(0, $n) as $j)
    {
        printf("% 4d", f($n, $n - $j - 1, $i) * $n + f($n, $j, $i) + 1);
    }
    echo "\n";
}
printf("\n Magic Constant: %d.\n", ($n * $n + 1) / 2 * $n);
