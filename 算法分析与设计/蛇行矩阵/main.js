require('readline').createInterface({
    input: process.stdin,
    output: process.stdout
}).on('line', (line) => {
    const r = Array(2).fill([...Array(+line).keys()]).reduce((a, b) => a.flatMap(x => b.map(y => x + [y]))),
        s = [...r].sort((a, b) => {
            const f = x => [+x[0] + +x[1], (+x[0] + +x[1]) % 2 ? -x[1] : x[1]], x = f(a), y = f(b);
            return x[0] === y[0] ? x[1] - y[1] : x[0] - y[0];
        }),
        m = r.map(x => s.indexOf(x) + 1);
    [...Array(+line).keys()].forEach(i => console.log(m.slice(i * +line, i * +line + (+line - i)).join()));
});
