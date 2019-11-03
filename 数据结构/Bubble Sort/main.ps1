function BubbleSort ($a) 
{
    $count = $a.Length - 1
    for ($swapped = $true; $swapped; --$count)
    {
        $swapped = $false
        for ($i = 0; $i -lt $count; ++$i) 
        {
            if ($a[$i] -gt $a[$i+1]) 
            {
                $a[$i], $a[$i+1] = $a[$i+1], $a[$i]
                $swapped = $true
            }
        }
    }
}
$test = (31,65,56,4564,1,3513,5468,46584,684,52)
BubbleSort($test)
$test
