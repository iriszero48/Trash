require('readline').createInterface({
    input: process.stdin
}).on('line', (line) => {
    const cin = line.split(" ").map(Number),
        bmi = cin[0] / Math.pow(cin[1], 2);
    console.log(bmi < 18.5 ? "Underweight" : bmi < 24 ? "Normal" : parseFloat(bmi.toPrecision(6)) + "\nOverweight")
});
