cc.Class({
    extends: cc.Component,

    properties: {
        par1_part2: {
            default: null,
            type: cc.AudioClip,
        },
    },

    start () {
        cc.audioEngine.playEffect(this.par1_part2, false);
    },
});
