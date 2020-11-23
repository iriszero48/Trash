let Long = require('long');

cc.Class({
    extends: cc.Component,

    properties: {
        exp: {
            default: '',
        },
    },

    setVal (x) {
        this.exp = x;
        this.node.children[0].getComponent(cc.Label).string = this.exp;
    },

    append (x) {
        if (/\d+/.test(x)) {
            this.exp += x;
        } else {
            this.exp += ' ' + x + ' ';
        }
        this.node.children[0].getComponent(cc.Label).string = this.exp;
    },

    eval () {
        if (this.exp === '') return;
        let n = Long.fromString('7797466619350050522', false);
        let values = this.exp.split(' ').filter(x => x !== '');
        let ops = {
            '+': (a, b) => a.add(b),
            '-': (a, b) => a.sub(b),
            '*': (a, b) => a.mul(b),
            '/': (a, b) => a.div(b),
        }
        let i = 0;
        while (values.length !== 1) {
            if (!/\d+/.test(values[i])) {
                values[i + 1] = ops[values[i]](Long.fromString(values[i - 1], false).mod(n), Long.fromString(values[i + 1], false).mod(n)).toUnsigned().mod(n).toString();
                values.splice(i - 1, 2);
                i--;
            } else {
                i++;
            }
        }
        values[0] = Long.fromString(values[0], false).mod(n).toString();
        this.setVal(values[0] === '7233170664317018478' ? 'dabendan' : values[0]);
    },

    clear() {
        this.setVal('');
    },

    start () {},
});
