<?php

function isSemiprime($x)
{
    $n = 0;
    for ($i = 2; $n < 3 && $x != 1;)
    {
        if (($x % $i) == 0)
        {
            $x /= $i;
            $n++;
        }
        else
        {
            $i++;
        }
    }
    return $n == 2;
}

foreach (range(0,100) as $x) echo (isSemiprime($x) ? "true" : "false") . "\n";
