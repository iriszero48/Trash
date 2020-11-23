cc.Class({
    extends: cc.Component,

    properties: {},

    start () {
        this.node.on(cc.Node.EventType.MOUSE_DOWN,  (e) => {
            console.info('set SideKeyFlag');
            cc.find("Main").getComponent('player').SideKeyFlag = true;
            this.node.destroy();
        }, this);
    },
});
