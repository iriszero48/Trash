let rgb = (...arg) => arg.map(x => x > 255 ? 'ff' : x < 0 ? '00' : x.toString(16).padStart(2,'0')).join('').toUpperCase()
