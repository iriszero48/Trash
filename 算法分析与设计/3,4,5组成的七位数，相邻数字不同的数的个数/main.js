Array(7)
        .fill([...Array(3).keys()].map(x => (x + 3).toString()))
        .reduce((a, b) => a.flatMap(x => b.map(y => x + y)))
        .filter(x => !/(\d)\1/.test(x))
        .map(Number)
        .length
