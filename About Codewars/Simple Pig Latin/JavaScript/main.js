const pigIt = str => str.split(' ').map(x =>/\W/.test(x) ? x : x.substr(1) + x[0] + 'ay').join(' ');
