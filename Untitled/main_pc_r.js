cc.Class({
    extends: cc.Component,

    properties: {
        btnAudio: {
            default: null,
            type: cc.AudioClip
        },
    },

    start () {
        this.node.on(cc.Node.EventType.MOUSE_DOWN,  (e) => {
            if (cc.find("Main").getComponent('player').UpsFlag &&
                !cc.find("Main").getComponent("player").MainPcFlag){
                cc.find("Main").getComponent('player').MainPcFlag = true;
                console.info("set MainPcFlag");
                cc.audioEngine.playEffect(this.btnAudio, false);
            }
        }, this);
    },
});