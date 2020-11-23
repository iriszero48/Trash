cc.Class({
    extends: cc.Component,

    properties: {
        prefab: {
            default: null,
            type: cc.Prefab,
        },
    },

    start () {
        this.node.on(cc.Node.EventType.MOUSE_DOWN,  (_) => {
            cc.find("Main/Right/Side").removeAllChildren();
            cc.find("Main/Right/Side").addChild(cc.instantiate(this.prefab));
        }, this);
    },
});
