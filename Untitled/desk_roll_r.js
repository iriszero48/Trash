cc.Class({
    extends: cc.Component,

    properties: {
        pool: {
            default: [],
        },
        target: {
            default: [],
        },
    },

    winQ () {
        let red = 0;
        let black = 0;
        let tmpTg = [...this.target];
        let sVal = [...Array(4).fill('')];
        for (let i = 0; i < 4; ++i) {
            sVal[i] = this.node.children[1].children[i].getComponent('desk_roll_btn').getVal();
        }
        for (let i = 0; i < 4; ++i) {
            if (sVal[i] === tmpTg[i]) {
                red++;
                sVal[i] = '';
                tmpTg[i] = '';
            }
        }
        for (let i = 0; i < 4; ++i) {
            if (sVal[i] === '') continue;
            let idx = tmpTg.indexOf(sVal[i]);
            if (idx !== -1) {
                black++;
                tmpTg = '';
            }
        }
        this.node.children[0].getComponent(cc.RichText).string = (red === 4 ?
            '<color=#000000>meishaonv</color>' :
            `${[...Array(red).fill('📀')].join('')}${[...Array(black).fill('💿')].join('')}`);
        if (red === 4) {
            cc.find("Main").getComponent('player').RollGameFlag = true;
            console.info('set RollGame');
            this.node.children[1].removeAllChildren();
        }
    },

    next (val) {
        return this.pool[(this.pool.indexOf(val) + 1) % this.pool.length];
    },

    onLoad () {
        this.pool = ['🎂', '🍕', '🍔', '🍟', '🍙', '🍜'];
        this.target = [...Array(4).fill('')];
        let tmpPool = [...this.pool];
        for (let i = 0; i < 4; ++i) {
            this.target[i] = tmpPool.splice(Math.floor(Math.random() * tmpPool.length), 1)[0];
        }
    },

    start () {
        if (cc.find("Main").getComponent('player').RollGameFlag) {
            this.node.children[0].getComponent(cc.RichText).string = '<color=#000000>meishaonv</color>';
            this.node.children[1].removeAllChildren();
        }
    },
});
