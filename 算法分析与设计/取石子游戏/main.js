require('readline').createInterface({
    input: process.stdin,
    output: process.stdout
}).on('line', (line) => {
    const x = line.split(" ").map(Number).sort().reverse();
    console.log(parseInt(x.reduce((a, b) => a - b) * (Math.sqrt(5) + 1) / 2) !== x[0]);
});
