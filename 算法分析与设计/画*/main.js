require('readline').createInterface({
    input: process.stdin,
    output: process.stdout
}).on('line', (line) => {
    const nm = +line / 2 + 1;
    [...Array(+line + 2).keys()].slice(1).forEach(x => console.log(" ".repeat(Math.abs(nm - x)) + "*".repeat(2*(nm - Math.abs(nm - x))-1)));
});
