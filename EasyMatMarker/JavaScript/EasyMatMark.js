class Mat {
    constructor(data) {
        this.data = data;
    }

    add(x) {
        for (let i = 0; i < 128 * 128; i++) {
            this.data[i] += x;
        }
    }

    div(x) {
        for (let i = 0; i < 128 * 128; i++) {
            this.data[i] /= x;
        }
    }
}

function mul(a, b, c) {
    for (let row = 0; row < 128; row++) {
        for (let col = 0; col < 128; col++) {
            let v = 0;
            for (let i = 0; i < 128; i++) {
                v += a[row * 128 + i] * b[i * 128 + col];
            }
            c[row * 128 + col] = v;
        }
    }
}

let a = new Mat(Array.from({length: 128 * 128}, (_, i) => (i + 1) / (128 * 128)));
// let a = new Mat(Array.from({length: 128 * 128}, (_, i) => Math.random()));
// a[127 * 128 + 127] = 1.;
let b = new Mat(Array.from({length: 128 * 128}, (_, i) => 1));
let c = new Mat(Array.from({length: 128 * 128}, (_, i) => 0));
let sum_value = 0;

const tp1 = Date.now();

console.log(a.data[128 * 128 - 1]);
for (let i = 0; i < 1; i++) {
    b.add(i);
    mul(a.data, b.data, c.data);
    console.log(c.data[128 * 128 - 1]);

    b.add(i + 1);
    mul(c.data, b.data, a.data);
    console.log(a.data[128 * 128 - 1]);

    a.div(c.data[127 * 128 + 127]);
    console.log(a.data[128 * 128 - 1]);
}

for (let i = 0; i < 128 * 128; ++i) {
    sum_value += a.data[i];
}

const tp2 = Date.now();

console.log(`${tp2 - tp1} ${sum_value}`);
