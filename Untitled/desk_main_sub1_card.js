cc.Class({
    extends: cc.Component,

    properties: {},

    onLoad () {
        if (cc.find("Main").getComponent('player').MoireFringesCardFlag) {
            this.node.destroy();
        }
    },

    start () {
        this.node.on(cc.Node.EventType.MOUSE_DOWN,  (e) => {
            cc.find("Main").getComponent('player').MoireFringesCardFlag = true;
            console.info('set MoireFringesCardFlag');
            this.node.destroy();
        }, this);
    },
});
