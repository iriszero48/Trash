cc.Class({
    extends: cc.Component,

    properties: {

    },

    start () {
        let octave = ['C', 'D', 'E', 'F', 'G', 'A', 'B'];
        let pool = [
            octave.map(x => x + '5'),
            octave.map(x => x + '6'),
            [octave[0] + '7'],
        ].flat();
        for (let i = 0; i < 15; ++i) {
            this.node.children[i].getComponent('keyboard_key').init(pool[i], this.node.parent.children[2].children[i].getComponent(cc.AudioSource));
        }
    },
});
