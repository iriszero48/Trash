cc.Class({
    extends: cc.Component,

    properties: {
        val: {
            default: '🎂',
        }
    },

    setVal (val) {
        this.val = val;
        this.node.children[0].children[0].getComponent(cc.Label).string = this.val;
    },

    getVal () {
        return this.node.children[0].children[0].getComponent(cc.Label).string;
    },

    start () {
        this.node.on(cc.Node.EventType.MOUSE_DOWN,  (e) => {
            this.setVal(this.node.parent.parent.getComponent('desk_roll_r').next(this.getVal()));
            this.node.parent.parent.getComponent('desk_roll_r').winQ();
        }, this);
    },
});
