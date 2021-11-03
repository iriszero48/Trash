function decode(v, key) {
    let v0=v[0];
    let v1=v[1];
    const delta=0x9E3779B9;
    let sum = delta * 0x20;
    for (let i = 0; i < 0x20; ++i)
    {
        v1 -= (v0 << 0x4 ^ v0 >>> 0x5) + v0 ^ sum + key[sum >>> 0xb & 0x3];
        sum -= delta;
        v0 -= (v1 << 0x4 ^ v1 >>> 0x5) + v1 ^ sum + key[sum & 0x3];
    }
    return [v0,v1];
}

// [...Array(100).keys()].forEach(x => clearInterval(x))

console.log([...Array(4).keys()]
  .map(i=>'6fbde674819a59bfa12092565b4ca2a7a11dc670c678681daf4afb6704b82f0c'.substr(i*16,16))
  .map(x=>[...Array(2).keys()].map(i=>Base16ToLong(x.substr(i*8,8))))
  .map(v=>decode(v, [909456177,825439544,892352820,926364468])
  .map(LongToStr4)
  .join(''))
  .join(''))
