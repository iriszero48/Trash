cc.Class({
    extends: cc.Component,

    properties: {
        editbox: {
            default: null,
            type: cc.Node,
        }
    },

    start () {
        console.info('load MonitorSideStage0');
        this.node.on(cc.Node.EventType.MOUSE_DOWN,  (e) => {
            console.info(`get ${this.editbox.getComponent(cc.EditBox).string}`);
            if (this.editbox.getComponent(cc.EditBox).string === "dabendan") {
                console.info('done MonitorSideStage0');
                cc.find("Main").getComponent('player').MonitorSideStage++;
                this.node.parent.parent.getComponent('monitor_side_r').stageLoad();
            }
        }, this);
    },
});
