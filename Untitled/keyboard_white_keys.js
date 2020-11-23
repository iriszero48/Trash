cc.Class({
    extends: cc.Component,

    properties: {

    },

    start () {
        let octave = ['C#', 'D#', 'F#', 'G#', 'A#'];
        let pool = [
            octave.map(x => x + '5'),
            octave.map(x => x + '6'),
        ].flat();
        let poolIndex = 0;
        for (let i = 0; i < 14; ++i) {
            if (this.node.children[i].opacity !== 0) {
                this.node.children[i].getComponent('keyboard_key').init(pool[poolIndex], this.node.parent.children[3].children[poolIndex].getComponent(cc.AudioSource));
                ++poolIndex;
            }
        }
    },
});
