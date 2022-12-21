<?php
$pw = $_POST["password"] ?? null;
$dev = $_POST["device"] ?? null;
$group = $_POST["group"] ?? null;
$session = $_POST["session"] ?? null;
$message = $_POST["message"] ?? null;
$time_beg = $_POST["time-begin"] ?? null;
$time_end = $_POST["time-end"] ?? null;
$skip = $_POST["skip"] ?? 0;
$limit = $_POST["limit"] ?? 1000;
?>

<style>
    table {
        width: 100%;
    }

    table tr td:nth-child(4),
    table tr td:nth-child(5) {
        white-space: nowrap;
    }

    table tr td:nth-child(6) {
        overflow-wrap: anywhere;
    }

    img {
        max-width: 100%;
        max-height: 100%;
    }
</style>

<form action="" method="post">
    <div>
        <label for="password">password</label>
        <input type="password" name="password" id="password" value="<?= $pw ?>" required>
    </div>

    <div>
        <label for="device">device</label>
        <input type="text" name="device" id="device" value="<?= $dev ?>">
    </div>

    <div>
        <label for="group">group</label>
        <input type="text" name="group" id="group" value="<?= $group ?>">
    </div>

    <div>
        <label for="session">session</label>
        <input type="text" name="session" id="session" value="<?= $session ?>">
    </div>

    <div>
        <label for="message">message</label>
        <input type="text" name="message" id="message" value="<?= $message ?>">
    </div>

    <div>
        <label for="time-begin">time &GreaterEqual;</label>
        <input type="text" name="time-begin" id="time-begin" value="<?= $time_beg ?>">

        <label for="time-end">time &lt;</label>
        <input type="text" name="time-end" id="time-end" value="<?= $time_end ?>">
    </div>

    <div>
        <label for="skip">skip</label>
        <input type="number" name="skip" id="skip" value="<?= $skip ?>">
    </div>

    <div>
        <label for="limit">limit</label>
        <input type="number" name="limit" id="limit" value="<?= $limit ?>">
    </div>

    <div>
        <input type="submit" value="query">
    </div>
</form>

<?php
class Param
{
    public string $name;

    function __construct($name)
    {
        $this->name = $name;
    }

    public function get_value(string $val): string
    {
        return $val;
    }
}

class BinParam extends Param
{
    public string $op;

    function __construct(string $name, string $op)
    {
        parent::__construct($name);
        $this->op = $op;
    }

    public function get_value(string $val): string
    {
        return sprintf('"%s" %s \'%s\'', $this->name, $this->op, addslashes($val));
    }
};

class WhereParam extends BinParam
{
    function __construct($name)
    {
        parent::__construct($name, '=');
    }
};

class TxtParam extends WhereParam
{
    public function get_value(string $val): string
    {
        return sprintf('"%s" @@ \'$[*].txt like_regex "%s"\'', $this->name, addslashes($val));
    }
}

if ($_SERVER['REQUEST_METHOD'] != "POST") return;

if (is_string($skip)) $skip = intval($skip);
if (is_string($limit)) $limit = intval($limit);

$dbconn = pg_connect("host=localhost dbname=qq_mht user=postgres password=" . $pw)
    or die('Could not connect: ' . pg_last_error());

$values = [];
$params = [];

if (!empty($dev)) {
    $params[] = new WhereParam('device');
    $values[] = $dev;
}

if (!empty($group)) {
    $params[] = new WhereParam('group');
    $values[] = $group;
}

if (!empty($session)) {
    $params[] = new WhereParam('session');
    $values[] = $session;
}

if (!empty($message)) {
    $params[] = new TxtParam('message');
    $values[] = $message;
}

if (!empty($time_beg)) {
    $params[] = new BinParam('time', '>=');
    $values[] = $time_beg;
}

if (!empty($time_end)) {
    $params[] = new BinParam('time', '<');
    $values[] = $time_end;
}

$query_where_str = join(" AND ", array_map(function ($i) use ($params, $values) {
    return $params[$i]->get_value($values[$i]);
}, array_keys($values)));


$query = sprintf('SELECT "device","group","session","time","object","message" 
FROM "records" 
WHERE %s 
ORDER BY "id" 
LIMIT %d 
OFFSET %d', $query_where_str, $limit, $skip);

var_dump($query);
echo '<br/>';

$result = pg_query($query) or die('Query failed: ' . pg_last_error());

const IMG_DIR = "\\\\Treediagram\\k\\qq_mht\\img";

class Record
{
    public string $dev;
    public string $group;
    public string $session;
    public string $time;
    public null | string $obj;
    public array $msg;

    function __construct(array $data_arr)
    {
        $this->dev = $data_arr['device'];
        $this->group = $data_arr['group'];
        $this->session = $data_arr['session'];
        $this->time = $data_arr['time'];
        $this->obj = $data_arr['object'];
        $this->msg = json_decode($data_arr['message']);
    }

    private function value_in_td(string $val): string
    {
        return "<td>$val</td>";
    }

    private function get_img_path(string $img): string
    {
        return IMG_DIR . '/' . substr($img, 0, 2) . '/' . $img;
    }

    private function get_img_src(string $img): string
    {
        if (str_starts_with($img, "{")) return $img;

        $path = $this->get_img_path($img);
        $type = pathinfo($path, PATHINFO_EXTENSION);
        $data = file_get_contents($path);
        return 'data:image/' . $type . ';base64,' . base64_encode($data);
    }

    public function print_in_tr()
    {
        foreach ([$this->dev, $this->group, $this->session, $this->time, $this->obj] as $c) {
            if (is_null($c)) {
                echo $this->value_in_td("<span style=\"color:gray\">NULL</span>");
                continue;
            }
            echo $this->value_in_td(nl2br(htmlspecialchars($c)));
        }

        echo $this->value_in_td(join("", array_map(function ($m) {
            if (isset($m->txt)) return nl2br(htmlspecialchars($m->txt));
            if (isset($m->img)) return '<img src="' . $this->get_img_src($m->img) . '"></img>';
            die("unknow key");
            var_dump($m);
        }, $this->msg)));
    }
}

echo '<table>';
while ($line = pg_fetch_array($result, null, PGSQL_ASSOC)) {
    echo "<tr>";

    $rec = new Record($line);
    $rec->print_in_tr();

    echo "</tr>";
}
echo "</table>";

pg_free_result($result);
pg_close($dbconn);
