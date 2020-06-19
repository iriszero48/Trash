Math.max.apply({},Array(6).fill([1,3,4,5,7,8]).reduce((a, b) => a.flatMap(x => b.map(y => x + [y]))).filter(x => !/(.).*?\1/.test(x)).filter(x => x%11==0))
