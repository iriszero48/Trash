cc.Class({
    extends: cc.Component,

    properties: {
        val: {
            default: '',
        },
        audio: {
            default: null,
            type: cc.AudioSource,
        },
    },

    init (val, audio) {
        this.val = val;
        this.audio = audio;
        this.node.on(cc.Node.EventType.MOUSE_DOWN,  (e) => {
            console.info(`get ${this.val}`);
            this.node.parent.parent.getComponent('keyboard_r').append(this.val);
            this.audio.play();
        }, this);
    },

    start () {},
});
