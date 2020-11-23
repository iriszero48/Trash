cc.Class({
    extends: cc.Component,

    properties: {},

    start () {
        this.node.on(cc.Node.EventType.MOUSE_DOWN,  (e) => {
            if (cc.find("Main").getComponent('player').DoorFlag) {
                console.info('done.');
                cc.director.loadScene('end');
            }
        }, this);
    },
});
