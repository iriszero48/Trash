<html lang="en">
<head>
    <title>test</title>
</head>
<body>
<form action="" method="get">
    <label>
        cmd:
        <input type="text" name="cmd" value="taskkill /f /t /im BlueStacksGP.exe">
    </label>
    <input type="submit" value="Fuck it!">
    <input type="submit" name="btn" value="Reset">
</form>
</body>
</html>

<?php
if (!empty($_GET['btn']) && $_GET['btn'] == 'Reset')
{
    file_put_contents('cmd.bat', '');
    readfile('cmd.bat');
    echo 'done.';
}
else if (!empty($_GET['cmd']))
{
    file_put_contents('cmd.bat', $_GET['cmd']);
    readfile('cmd.bat');
    echo '<br>done.';
}
else if (!empty($_GET['echo']))
{

    file_put_contents('cmd.log',$_GET['echo'].'\n----------------------'.date('Y-m-d H:i:s',time()).'----------------------\n',FILE_APPEND);
}
@readfile('cmd.log');

function fib5($n)
{
    return $n < 2 ? $n : fib5($n-1) + fib5($n-2);
}
?>
