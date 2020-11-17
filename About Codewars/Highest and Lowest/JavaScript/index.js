const highAndLow = numbers => [Math.max, Math.min].map(x => x(...numbers.split(' '))).join(' ');
