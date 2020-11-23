cc.Class({
    extends: cc.Component,

    properties: {
        chessman: {
            default: null,
            type: cc.Prefab,
        },
        selected: {
            default: 0,
        }
    },

    moveQ (index, sIndex, sVal) {
        let x = index % 4;
        let y = Math.floor(index / 4);
        let sx = sIndex % 4;
        let sy = Math.floor(sIndex / 4);
        if (sVal === 'K' || sVal === 'Q' || sVal === 'B') {
            if (x === sx - 1 && y === sy - 1) return true;
            if (x === sx - 1 && y === sy + 1) return true;
            if (x === sx + 1 && y === sy + 1) return true;
            if (x === sx + 1 && y === sy - 1) return true;
        }
        if (sVal === 'K' || sVal === 'Q' || sVal === 'R') {
            if (x === sx && y === sy - 1) return true;
            if (x === sx && y === sy + 1) return true;
            if (x === sx + 1 && y === sy) return true;
            if (x === sx - 1 && y === sy) return true;
        }
        if (sVal === 'N') {
            if (x === sx - 1 && y === sy - 2) return true;
            if (x === sx - 1 && y === sy + 2) return true;
            if (x === sx + 1 && y === sy + 2) return true;
            if (x === sx + 1 && y === sy - 2) return true;
            if (x === sx - 2 && y === sy - 1) return true;
            if (x === sx - 2 && y === sy + 1) return true;
            if (x === sx + 2 && y === sy + 1) return true;
            if (x === sx + 2 && y === sy - 1) return true;
        }
        return false;
    },

    winQ () {
        for (let i = 0; i < 4; ++i) {
            if (this.node.children[i].getComponent('monitor_side_stage1_btn').val !== 'N') {
                return false;
            }
        }
        return true;
    },

    select (index) {
        let val = this.node.children[index].getComponent('monitor_side_stage1_btn').val;
        if (this.selected === null) {
            if (val === ' ') return;
            this.selected = index;
            this.node.children[index].getComponent('monitor_side_stage1_btn').setSelect();
        } else if (index === this.selected) {
            this.selected = null;
            this.node.children[index].getComponent('monitor_side_stage1_btn').unsetSelect();
        } else if (val === ' ') {
            if (this.moveQ(index, this.selected, this.node.children[this.selected].getComponent('monitor_side_stage1_btn').val)) {
                this.node.children[index].getComponent('monitor_side_stage1_btn').setVal(this.node.children[this.selected].getComponent('monitor_side_stage1_btn').val);
                this.node.children[this.selected].getComponent('monitor_side_stage1_btn').setVal(' ');
                this.selected = null;
            }
        }
        if (this.winQ()) {
            console.info('done MonitorSideStage1');
            console.info('set SidePcGameFlag');
            cc.find("Main").getComponent('player').SidePcGameFlag = true;
            cc.find("Main").getComponent('player').MonitorSideStage++;
            this.node.parent.getComponent('monitor_side_r').stageLoad();
        }
    },

    start () {
        this.selected = null;
        console.info('load MonitorSideStage1');
        let pool = [
            [...Array(1).fill(' ')],
            [...Array(2).fill('K')],
            [...Array(2).fill('Q')],
            [...Array(3).fill('B')],
            [...Array(4).fill('R')],
        ].flat();
        let index = 0;
        while (pool.length !== 0) {
            let cm = cc.instantiate(this.chessman);
            cm.getComponent('monitor_side_stage1_btn').initVal(index++, pool.splice(Math.floor(Math.random() * pool.length), 1)[0]);
            this.node.addChild(cm);
        }
        for (let _ = 0; _ < 4; ++_) {
            let cm = cc.instantiate(this.chessman);
            cm.getComponent('monitor_side_stage1_btn').initVal(index++, 'N');
            this.node.addChild(cm);
        }
    },
});
