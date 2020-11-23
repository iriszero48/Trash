cc.Class({
    extends: cc.Component,

    properties: {
        moon: {
            default: null,
            type: cc.Prefab,
        },
        sun: {
            default: null,
            type: cc.Prefab,
        },
    },

    swap (x, y) {
        const getIndex = (x, y) => y * 5 + x;
        const getVal = (i) => this.node.children[0].children[i].getComponent('monitor_stage1_moon').val;
        const set = (x0, y0, x1, y1) => this.node.children[0].children[getIndex(x0, y0)].getComponent('monitor_stage1_moon').setVal(getVal(getIndex(x1, y1)));
        let tempVal = getVal(getIndex(x, y));
        set(x + 0, y + 0, x + 0, y + 1);
        set(x + 0, y + 1, x + 1, y + 1);
        set(x + 1, y + 1, x + 1, y + 0);
        this.node.children[0].children[getIndex(x + 1, y)].getComponent('monitor_stage1_moon').setVal(tempVal);
        for (let y = 0; y < 5; ++y) {
            for (let x = 0; x < 4; ++x) {
                if (getVal(getIndex(x, y)) !== getVal(getIndex(x + 1, y))) {
                    return false;
                }
            }
        }
        return true;
    },

    start () {
        console.info("load MonitorStage1");
        let pool = ['🌑', '🌒', '🌓', '🌔', '🌕'].flatMap(x => [...Array(5)].fill(x));
        //let pool = ['1', '2', '3', '4', '5'].flatMap(x => [...Array(5)].fill(x));
        for (let x = 0; x < 5 * 5; ++x) {
            let moon = cc.instantiate(this.moon);
            moon.index = x;
            moon.getComponent('monitor_stage1_moon').setVal(`<color=#FFC83D>${pool.splice(Math.floor(Math.random() * pool.length), 1)[0]}</color>`);
            this.node.children[0].addChild(moon)
        }
        for (let x = 0; x < 4 * 4; ++x) {
            let sun = cc.instantiate(this.sun);
            sun.getComponent('monitor_stage1_sum').setBoard(x, this.node);
            this.node.children[1].addChild(sun);
        }
    },
});
