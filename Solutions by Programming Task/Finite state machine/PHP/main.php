<?php
$status = 'ready';

function getOp($tip)
{
    echo $tip . "\n" . '<- ';
    return strtolower(fgets(STDIN))[0];
};

while ($status != 'exit')
{
    switch ($status)
    {
        case 'ready':
            $op = getOp("ready.\ndeposit/quit");
            if ($op == 'd') $status = 'waiting';
            elseif ($op == 'q') $status = 'exit';
            break;
        case 'waiting':
            $op = getOp("waiting.\nselect/refund");
            if ($op == 's') $status = 'dispense';
            elseif ($op == 'r') $status = 'refunding';
            break;
        case 'dispense':
            if (getOp("dispense.\nremove") == 'r') $status = 'ready';
            break;
        case 'refunding':
            $status = 'ready';
            break;
    }
}

echo 'exit.';
