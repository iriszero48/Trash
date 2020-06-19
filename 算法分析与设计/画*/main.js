require('readline').createInterface({
    input: process.stdin
}).on('line', (line) => {
    const nm = Math.floor(+line / 2 + 1);
    [...Array(+line + 1).keys()].slice(1).forEach(x => console.log(" ".repeat(Math.abs(nm - x)) + "*".repeat(2 * (nm - Math.abs(nm - x)) - 1)));
});
