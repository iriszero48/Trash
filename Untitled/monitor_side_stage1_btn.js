cc.Class({
    extends: cc.Component,

    properties: {
        index: {
            default: 0,
        },
        val: {
            default: ' ',
        },
    },

    setSelect () {
        this.node.children[0].color = cc.Color.set(this.node.children[0].color, 51, 51, 51);
    },

    unsetSelect () {
        this.node.children[0].color = cc.Color.WHITE;
    },

    setVal(val) {
        this.val = val;
        this.node.children[0].children[0].getComponent(cc.Label).string = val;
        this.unsetSelect();
    },

    initVal(index, val) {
        this.index = index;
        this.setVal(val);
        this.node.on(cc.Node.EventType.MOUSE_DOWN,  (e) => {
            this.node.parent.getComponent('monitor_side_stage1').select(this.index);
        }, this);
    },

    start () {},
});
