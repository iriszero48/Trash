<?php
const languagePath = 'language.txt';
const sourcePath = 'source.txt';
const taskPath = 'task.txt';

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
