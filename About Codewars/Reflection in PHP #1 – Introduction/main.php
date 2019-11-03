function get_info($fn)
{
    $r = new ReflectionFunction($fn);
    return [$r->getNumberOfParameters(),
        $r->getNumberOfRequiredParameters(),
        $r->hasReturnType(),
        $r->isClosure(),
        $r->isInternal(),
        $r->isUserDefined()];
}
