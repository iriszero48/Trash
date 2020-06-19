Array(5).fill([1,2,5,7,8,9].map(x => x.toString())).reduce((a, b) => a.flatMap(x => b.map(y => x + y))).filter(x => !/(.).*?\1/.test(x)).filter(x => x%75==0).length
