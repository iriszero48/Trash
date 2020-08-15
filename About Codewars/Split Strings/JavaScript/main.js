function solution(str){
  const paded = str.padEnd(str.length + (str.length & 1), '_');
  return [...Array(paded.length / 2).keys()].map(i => paded.substring(i*2,i*2+2))
}
