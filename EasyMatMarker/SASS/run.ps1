Measure-Command { start-process SASS\main.bat -wait } > SASS\cost.txt

$cost = (Get-Content SASS\cost.txt | Select-String -Pattern "TotalMilliseconds : (\d+\.\d+)").matches.groups[1].Value
$result = (Get-Content SASS\result.txt | Select-String -Pattern "DEBUG: (\d+\.\d+)").matches.groups[1].Value

"$cost $result"
