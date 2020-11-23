cc.Class({
    extends: cc.Component,

    properties: {
        index: {
            default: 0,
        },
        board: {
            default: null,
            type: cc.Node,
        },
    },

    setBoard (index, board) {
        this.index = index;
        this.board = board;
        this.node.on(cc.Node.EventType.MOUSE_DOWN,  (_) => {
            let x = this.index % 4;
            let y = Math.floor(this.index / 4);
            if (this.board.getComponent('monitor_stage1').swap(x, y)) {
                console.info('done MonitorStage1');
                console.info('set MainPcGameFlag');
                cc.find("Main").getComponent('player').MainPcGameFlag = true;
                cc.find("Main").getComponent('player').MonitorStage++;
                this.board.parent.getComponent('monitor_r').stageLoad();
            }
        }, this);
    },
});
