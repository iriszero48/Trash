function iter-pi
{
    [OutputType([string])]
    Param ([double]$epsilon)
    $M_PI = 3.14159265358979323846
    $res, $i = 0.0, 0
    while ([Math]::Abs($res * 4 - $M_PI) -gt $epsilon)
    {
        $res += [Math]::Pow(-1, $i) / (2 * $i + 1)
        $i++
    }
    return [String]::Format("[{0:F0}, {1:F10}]", $i, $res * 4);
}
