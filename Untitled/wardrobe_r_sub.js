cc.Class({
    extends: cc.Component,

    properties: {
        wardrobe_r_sub_r: {
            default: null,
            type: cc.Prefab,
        }
    },

    start () {
        this.node.on(cc.Node.EventType.MOUSE_DOWN,  (e) => {
            if (cc.find("Main").getComponent('player').WardrobeKeyFlag) {
                this.node.parent.parent.addChild(cc.instantiate(this.wardrobe_r_sub_r));
                this.node.parent.getComponent('wardrobe_r').kill();
            }
        }, this);
    },
});
