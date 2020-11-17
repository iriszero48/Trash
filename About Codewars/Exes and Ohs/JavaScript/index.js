const XO = str => [/x/i, /o/i].map(x => str.split(x).length).reduce((a, b) => a === b);
