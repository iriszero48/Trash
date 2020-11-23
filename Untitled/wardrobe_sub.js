cc.Class({
    extends: cc.Component,

    properties: {
        moire_fringes_mix: {
            default: null,
            type: cc.Prefab,
        },
    },

    start () {
        this.node.on(cc.Node.EventType.MOUSE_DOWN,  (e) => {
            if (cc.find("Main").getComponent('player').MoireFringesCardFlag &&
                !cc.find("Main").getComponent('player').SideKeyFlag) {
                this.node.parent.addChild(cc.instantiate(this.moire_fringes_mix));
                this.node.destroy();
            }
        }, this);
    },
});
