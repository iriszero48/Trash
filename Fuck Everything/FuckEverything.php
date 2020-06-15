<?php
const languagePath = 'K:\z 181009-181202\新建文件夹 (197)\FuckEverything\language.txt';
const sourcePath = 'K:\z 181009-181202\新建文件夹 (197)\FuckEverything\source.txt';
const taskPath = 'K:\z 181009-181202\新建文件夹 (197)\FuckEverything\task.txt';

$fileReadLines = function ($x) { return file($x, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES); };
$randomSelect = function ($x) { return $x[random_int(0, count($x) - 1)]; };

$language = $fileReadLines(languagePath);
$source = $fileReadLines(sourcePath);
$task = $fileReadLines(taskPath);

echo 'Fuck Everything.';

while (true)
{
    fgets(STDIN);
    $s = $randomSelect($source);
    echo '-> ' . $s . ' <=> ' . $randomSelect($language) . ($s == 'rosettacode' ? ' <=> ' . $randomSelect($task) : '') . '.';
}
