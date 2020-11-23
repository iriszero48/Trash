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
            if (cc.find("Main").getComponent('player').PowerFlag &&
                !cc.find("Main").getComponent("player").SidePcFlag){
                cc.find("Main").getComponent('player').SidePcFlag = true;
                console.info("set SidePcFlag");
                cc.audioEngine.playEffect(this.btnAudio, false);
            }
        }, this);
    },
});