<?php
$cin = intval(fgets(STDIN));
echo $cin * 5 < 11 + $cin * 3 ? 'Local' : 'Luogu';
