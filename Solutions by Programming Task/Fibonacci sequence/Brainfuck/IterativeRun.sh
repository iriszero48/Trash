printf \\$(printf '%03o' `read x && echo $x`) | bf Iterative.bf | od -t d1 -A none
