cc.Class({
    extends: cc.Component,

    properties: {
        editbox: {
            default: null,
            type: cc.Node,
        }
    },

    start () {
        console.info('load MonitorStage0');
        this.node.on(cc.Node.EventType.MOUSE_DOWN,  (e) => {
            console.info(`get ${this.editbox.getComponent(cc.EditBox).string}`);
            if (this.editbox.getComponent(cc.EditBox).string === "meishaonv") {
                console.info('done MonitorStage0');
                cc.find("Main").getComponent('player').MonitorStage++;
                this.node.parent.parent.getComponent('monitor_r').stageLoad();
            }
        }, this);
    },
});
