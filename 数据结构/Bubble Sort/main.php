<?php
    function BubbleSort(array &$array){
        $count = count($array) - 1;
        for($swapped = true;$swapped;--$count){
            $swapped = false;
            for ($i = 0; $i < $count; ++$i){
                if ($array[$i] > $array[$i + 1]){
                    list($array[$i + 1], $array[$i]) = array($array[$i], $array[$i + 1]);
                    $swapped = true;
                }
            }
        }
        return $array;
    }
    $test = array(32132,132,132,132,165,5646,4897,4564,654986,4651651,2313212312,2);
    print_r(BubbleSort($test));
?>
