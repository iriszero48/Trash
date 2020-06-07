<?php
function IsEven($x)
{
    return ($x & 1) == 0;
}

function IsOdd($x)
{
    return ($x & 1) != 0;
}
