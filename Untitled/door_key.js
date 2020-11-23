cc.Class({
    extends: cc.Component,

    properties: {},

    onLoad () {
        if (cc.find("Main").getComponent('player').DoorFlag) {
            this.node.destroy();
        }
    },

    start () {
        this.node.on(cc.Node.EventType.MOUSE_DOWN,  (e) => {
            cc.find("Main").getComponent('player').DoorFlag = true;
            console.info('set DoorFlag');
            this.node.destroy();
        }, this);
    },
});
