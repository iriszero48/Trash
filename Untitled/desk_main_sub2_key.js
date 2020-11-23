cc.Class({
    extends: cc.Component,

    properties: {},

    onLoad () {
        if (cc.find("Main").getComponent('player').WardrobeKeyFlag) {
            this.node.destroy();
        }
    },

    start () {
        this.node.on(cc.Node.EventType.MOUSE_DOWN,  (e) => {
            cc.find("Main").getComponent('player').WardrobeKeyFlag = true;
            console.info('set WardrobeKeyFlag');
            this.node.destroy();
        }, this);
    },
});
