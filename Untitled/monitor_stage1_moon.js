cc.Class({
    extends: cc.Component,

    properties: {
        index: {
            default: 0,
        },
        val: {
            default: '❌',
        },
    },

    setVal (val) {
        this.val = val;
        this.node.getComponent(cc.RichText).string = this.val;
    },

    start () {
        this.setVal(this.val);
    },
});
