[...Array(200 - (7 * 8 - 5)).keys()].map(x=>x + (7 * 8 - 5)).filter((_, i) => i % 56 === 0).filter(x => x % 9 === 1)
