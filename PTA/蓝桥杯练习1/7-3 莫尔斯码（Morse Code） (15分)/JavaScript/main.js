let codes = new Map(Object.entries({
    A: '.-', B: '-...', C: '-.-.', D: '-..', E: '.', F: '..-.',
    G: '--.', H: '....', I: '..', J: '.---', K: '-.-', L: '.-..',
    M: '--', N: '-.', O: '---', P: '.--.', Q: '--.-', R: '.-.',
    S: '...', T: '-', U: '..-', V: '...-', W: '.--', X: '-..-',
    Y: '-.--', Z: '--..', 0: '-----', 1: '.----', 2: '..---', 3: '...--',
    4: '....-', 5: '.....', 6: '-....', 7: '--...', 8: '---..', 9: '----.'
}));

let start = 0;

require('readline').createInterface({
    input: process.stdin
}).on('line', (line) => {
    if (start === 0) {
        start = +line;
    } else {
        console.log(/[.-]/.test(line) ? line.split('|').map(x => x.split(' ').map(y => [...codes].find(([_, v]) => v === y)[0]).join('')).join(' ') : line.split(' ').map(x => [...x].map(y => codes.get(y)).join(' ')).join('|'));
    }
});
