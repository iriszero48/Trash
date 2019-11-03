function powersOfTwo(n){
  return new Array(n+1).fill(0).map((el, i) => i).map(x => Math.pow(2,x));
}
