cc.Class({
    extends: cc.Component,

    properties: {
        monitorStage0: {
            default: null,
            type: cc.Prefab,
        },
        monitorStage1: {
            default: null,
            type: cc.Prefab,
        },
        monitorStage2: {
            default: null,
            type: cc.Prefab,
        },
    },

    stageLoad () {
        this.node.removeAllChildren();
        this.node.addChild(cc.instantiate(({
                    0: this.monitorStage0,
                    1: this.monitorStage1,
                    2: this.monitorStage2,
                })[cc.find("Main").getComponent('player').MonitorSideStage]));
    },

    start () {
        if (cc.find("Main").getComponent('player').SidePcFlag){
            this.stageLoad();
        }
    },
});