<?php
$id_file = [];

function vc($filename) {
    $f =fopen($filename, "rb");
    if($f){
        fseek($f,++$pos1);
        $buf =fread($f,1);
        $int1 =ord($buf);
        fseek($f,++$pos2);
        $buf =fread($f,1);
        $int2 =ord($buf);
        fseek($f,++$pos3);
        $buf =fread($f,1);
        $int3 =ord($buf);
        fseek($f,++$pos4);
        $buf =fread($f,1);
        $int4 =ord($buf);
        fclose($f);
    }
}

foreach ($id_file as $id => $filename) {
    echo $id . ' ';
    vcode($filename);
}
