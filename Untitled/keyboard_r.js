cc.Class({
    extends: cc.Component,

    properties: {
        target: {
            default: '',
        },
        played: {
            default: '',
        },
    },

    append(x) {
        this.played += x;
        if (this.played.indexOf(this.target) !== -1) {
            if (!cc.find("Main").getComponent('player').SightReadingFlag) {
                cc.find("Main").getComponent('player').SightReadingFlag = true;
                cc.find("Main").getComponent('player').MonitorStage++;
                console.info('set SightReadingFlag');
                this.initTarget();
            } else {
                cc.find("Main").getComponent('player').AllLast = true;
                console.info('set AllLast');
            }
        }
    },

    initTarget () {
        this.target = cc.find("Main").getComponent('player').SightReadingFlag ? 'A6G6C7G6F6E6F6E6' : 'C6E6G6B5C6D6C6';
    },

    start () {
        this.initTarget();
    },
});
