cc.Class({
    extends: cc.Component,

    properties: {
        monitor_tip: {
            default: null,
            type: cc.Prefab,
        },
        door_key: {
            default: null,
            type: cc.Prefab,
        },
        mainDeskKey: {
            default: null,
            type: cc.Prefab,
        },
        par1_part1: {
            default: null,
            type: cc.Prefab,
        },
    },

    start () {
        this.node.removeAllChildren();
        if (cc.find("Main").getComponent('player').AllLast) {
            this.node.addChild(cc.instantiate(this.door_key));
        } else if (cc.find("Main").getComponent('player').SightReadingFlag) {
            this.node.addChild(cc.instantiate(this.monitor_tip));
        } else if (cc.find("Main").getComponent('player').MainPcGameFlag &&
            !cc.find("Main").getComponent('player').MainDeskKeyFlag) {
            this.node.addChild(cc.instantiate(this.mainDeskKey));
        } else if (cc.find("Main").getComponent('player').SidePcGameFlag) {
            this.node.addChild(cc.instantiate(this.par1_part1));
        }
    },
});
