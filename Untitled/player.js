cc.Class({
    extends: cc.Component,

    properties: {
        DoorFlag: { default: false, },
        PowerFlag: { default: false, },
        UpsFlag: { default: false, },
        RollGameFlag: { default: false, },
        MainPcFlag: { default: false, },
        MainPcGameFlag: { default: false, },
        MainDeskKeyFlag: { default: false, },
        MoireFringesCardFlag: { default: false, },
        WardrobeKeyFlag: { default: false, },
        SideKeyFlag: { default: false, },
        SidePcFlag: { default: false, },
        SidePcGameFlag: { default: false, },
        SightReadingFlag: { default:false, },
        AllLast: { default: false, },

        MonitorStage: { default: 0, },
        MonitorSideStage: { default: 0 },
    },

    start () {},
});
