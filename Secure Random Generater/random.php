<?php
$begin = $_GET['begin'] ?? 1;
$end = $_GET['end'] ?? 6;
$length = $_GET['length'] ?? 1;
echo json_encode(array('data' => array_map(function () use (&$begin, $end){return random_int($begin, $end);}, range(1, $length))));
