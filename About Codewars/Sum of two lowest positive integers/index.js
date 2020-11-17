const sumTwoSmallestNumbers = numbers => numbers.sort((a, b) => b - a).slice(-2).reduce((a, b) => a + b);
