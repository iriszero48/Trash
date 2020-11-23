cc.Class({
    extends: cc.Component,

    properties: {},

    onLoad () {
        if (!cc.find("Main").getComponent('player').MainDeskKeyFlag) {
            this.node.destroy();
        }
    },
});
