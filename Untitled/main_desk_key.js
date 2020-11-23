cc.Class({
    extends: cc.Component,

    properties: {},

    start () {
        this.node.on(cc.Node.EventType.MOUSE_DOWN,  (e) => {
            cc.find("Main").getComponent('player').MainDeskKeyFlag = true;
            console.info('set MainDeskKeyFlag');
            this.node.destroy();
        }, this);
    },
});
