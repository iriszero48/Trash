#!/bin/bash

fib() 
{
    p=0
    n=1
    for ((i=0;i<$1;++i)) do
        sum=$(($p+$n))
        p=$n
        n=$sum
    done
    echo $p
    return $p
}
