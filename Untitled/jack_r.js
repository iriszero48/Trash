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
            if (!cc.find("Main").getComponent('player').PowerFlag){
                cc.find("Main").getComponent('player').PowerFlag = true;
                console.info("set PowerFlag");
                cc.audioEngine.playEffect(this.btnAudio, false);
            }
        }, this);
    },
});
