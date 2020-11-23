cc.Class({
    extends: cc.Component,

    properties: {},

    onLoad () {
        if (!cc.find("Main").getComponent('player').SideKeyFlag) {
            this.node.destroy();
        }
    },
});
