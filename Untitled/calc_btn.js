cc.Class({
    extends: cc.Component,

    properties: {},

    start () {
        this.node.on(cc.Node.EventType.MOUSE_DOWN,  (e) => {
            let val = this.node.children[0].children[0].getComponent(cc.Label).string;
            if (val === '=') {
                this.node.parent.parent.getComponent('calc').eval();
            } else if (val === 'C') {
                this.node.parent.parent.getComponent('calc').clear();
            } else {
                this.node.parent.parent.getComponent('calc').append(val);
            }
        }, this);
    },
});
