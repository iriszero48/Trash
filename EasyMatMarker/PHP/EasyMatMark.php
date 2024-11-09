<?php
$a = array_fill(0, 128*128, 0.);
$b = array_fill(0, 128*128, 1.);
$c = array_fill(0, 128*128, 0.);

for ($i = 0; $i < 128 * 128; $i++) {
    // $a[$i] = ($i + 1.) / (128 * 128);
    $a[$i] = mt_rand() / mt_getrandmax();
}
$a[128 * 128 - 1] = 1.0;

function add(& $result, float $x): void
{
    for ($i = 0; $i < 128 * 128; $i++) {
        $result[$i] += $x;
    }
}

function div(& $result, float $x): void
{
    for ($i = 0; $i < 128 * 128; $i++) {
        $result[$i] /= $x;
    }
}

function mul($a, $b, & $c): void
{
    for ($row = 0; $row < 128; $row++) {
        for ($col = 0; $col < 128; $col++) {
            $v = 0.;
            for ($i = 0; $i < 128; $i++) {
                $v += $a[$row * 128 + $i] * $b[$i * 128 + $col];
            }
            $c[$row * 128 + $col] = $v;
        }
    }
}

$tp1 = hrtime(true);

for ($i = 0; $i < 1; $i++) {
    add($b, $i);
    mul($a, $b, $c);

    add($b, $i + 1.);
    mul($c, $b, $a);

    div($a, $c[127 * 128 + 127]);
}

$sum_val = array_sum($a);

$tp2 = hrtime(true);

$duration = ($tp2 - $tp1) / 1000. / 1000.;
echo "$duration $sum_val";
