let lookAndSay = (d, n) => [...Array(n + 1).fill(d + "")].reduce(p => p.replace(/(.)\1*/g, w => w.length + w[0]))
