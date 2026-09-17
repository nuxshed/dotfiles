pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import "../../components"
import "../../config"
import "../../services"
import "../files"

ColumnLayout {
    id: root

    readonly property bool charging: SysMon.batStatus === "Charging"
    readonly property bool discharging: SysMon.batStatus === "Discharging"
    readonly property color accent: charging ? Colors.batteryCharging : discharging ? Colors.batteryDischarging : Colors.batteryNotCharging
    readonly property var profiles: ["Quiet", "Balanced", "Performance"]
    readonly property var apps: SysMon.batAppList.slice(0, 7)
    readonly property string sessionText: {
        if (SysMon.batSince === 0)
            return "No data yet";
        const s = SysMon.batSession;
        const delta = s.length > 1 ? s[s.length - 1][1] - s[0][1] : 0;
        if (SysMon.batOnBattery)
            return `${SysMon.fmtDuration((Date.now() - SysMon.batSince) / 1000)} on battery · ${-delta}% used`;
        return `${SysMon.fmtDuration((Date.now() - SysMon.batSince) / 1000)} plugged in · ${delta >= 0 ? "+" : ""}${delta}%`;
    }
    readonly property string headline: {
        if (!SysMon.batPresent)
            return "No battery";
        if (charging)
            return `${SysMon.fmtHours(SysMon.batTimeLeft)} until full`;
        if (discharging)
            return `${SysMon.fmtHours(SysMon.batTimeLeft)} remaining`;
        if (SysMon.batStatus === "Full")
            return "Fully charged";
        if (SysMon.acOnline && SysMon.batThreshold < 100 && SysMon.batCapacity >= SysMon.batThreshold - 1)
            return "Charged to limit";
        return SysMon.acOnline ? "Plugged in" : SysMon.batStatus;
    }
    readonly property string statusText: {
        if (!SysMon.batPresent)
            return "No battery detected";
        if (charging)
            return `Charging at ${SysMon.batPower.toFixed(1)} W`;
        if (discharging)
            return `On battery · drawing ${SysMon.batPower.toFixed(1)} W`;
        if (SysMon.acOnline && SysMon.batThreshold < 100 && SysMon.batCapacity >= SysMon.batThreshold - 1)
            return `Plugged in · held at the ${SysMon.batThreshold}% charge limit`;
        return SysMon.acOnline ? "Plugged in · not charging" : SysMon.batStatus;
    }
    readonly property string detailText: `${SysMon.batEnergyNow.toFixed(1)} of ${SysMon.batEnergyFull.toFixed(1)} Wh · ${SysMon.batVoltage.toFixed(2)} V · health ${SysMon.batHealth.toFixed(0)}%`

    spacing: 12

    Rectangle {
        Layout.fillWidth: true
        Layout.fillHeight: false
        implicitHeight: 136
        radius: 10
        color: Colors.surfaceActive
        border.width: 1
        border.color: Colors.outline

        RowLayout {
            anchors.fill: parent
            anchors.margins: 20
            spacing: 24

            BatteryShape {
                Layout.preferredWidth: 200
                Layout.preferredHeight: 88
                level: SysMon.batCapacity
                limit: SysMon.batThreshold
                accent: root.accent
                charging: root.charging
            }

            Column {
                Layout.fillWidth: true
                spacing: 4

                Text {
                    text: root.headline
                    color: Colors.textBright
                    font.pixelSize: 18
                    font.family: Fonts.family
                    font.weight: Font.Medium
                }
                Text {
                    text: root.statusText
                    color: Colors.textDimmed
                    font.pixelSize: 12
                    font.family: Fonts.family
                }
                Text {
                    text: root.detailText
                    color: Colors.textMuted
                    font.pixelSize: 11
                    font.family: Fonts.family
                }
            }

            Column {
                Layout.preferredWidth: 240
                spacing: 6
                visible: Asusctl.isAvailable && SysMon.batPresent

                RowLayout {
                    width: parent.width

                    Text {
                        Layout.fillWidth: true
                        text: "Charge limit"
                        color: Colors.textMuted
                        font.pixelSize: 11
                        font.family: Fonts.family
                    }
                    Text {
                        text: `${limitSlider.pending}%`
                        color: Colors.textBright
                        font.pixelSize: 11
                        font.family: Fonts.family
                        font.weight: Font.Medium
                    }
                }
                Slider {
                    id: limitSlider
                    width: parent.width
                    from: 20
                    to: 100
                    step: 5
                    value: SysMon.batThreshold > 0 ? SysMon.batThreshold : 100
                    accent: root.accent
                    onCommitted: v => SysMon.setChargeLimit(v)
                }
                Text {
                    text: SysMon.batThreshold < 100 ? "Full charge once ›" : "Limit disabled"
                    color: fullMouse.containsMouse ? Colors.textBright : Colors.textDimmed
                    font.pixelSize: 11
                    font.family: Fonts.family

                    MouseArea {
                        id: fullMouse
                        anchors.fill: parent
                        enabled: SysMon.batThreshold < 100
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        onClicked: SysMon.chargeFullOnce()
                    }
                }
            }
        }
    }

    RowLayout {
        Layout.fillWidth: true
        Layout.fillHeight: true
        spacing: 12

        Card {
            Layout.fillWidth: true
            Layout.fillHeight: true
            title: "Power draw"
            subtitle: SysMon.batSmoothPower > 0 ? `${SysMon.batSmoothPower.toFixed(1)} W average over 30 s` : "No current flowing"
            value: `${SysMon.batPower.toFixed(1)} W`
            accent: root.accent
            series: [{ data: SysMon.batPowerHist, color: root.accent }]
            max: 0
            fmt: v => `${v.toFixed(1)} W`
        }

        Card {
            Layout.fillWidth: true
            Layout.fillHeight: true
            title: SysMon.batOnBattery ? "Since unplugged" : "Since plugged in"
            subtitle: root.sessionText
            value: SysMon.batSince > 0 ? (SysMon.batOnBattery ? `${SysMon.batAppTotal.toFixed(1)} Wh` : SysMon.fmtDuration((Date.now() - SysMon.batSince) / 1000)) : "—"
            accent: root.accent
            series: [{ data: SysMon.batSession.map(s => s[1]), color: root.accent, span: 0 }]
            span: 0
        }
    }

    RowLayout {
        Layout.fillWidth: true
        Layout.fillHeight: false
        spacing: 12

        Panel {
            Layout.fillWidth: true
            Layout.fillHeight: true
            Layout.preferredWidth: 1
            title: SysMon.batOnBattery ? "App usage · battery" : "App usage · CPU time (plugged in)"

            Text {
                visible: root.apps.length === 0
                text: "Collecting…"
                color: Colors.textMuted
                font.pixelSize: 11
                font.family: Fonts.family
            }

            Repeater {
                model: root.apps

                RowLayout {
                    id: appRow
                    required property var modelData
                    readonly property real share: SysMon.batOnBattery ? (SysMon.batAppTotal > 0 ? modelData.wh / SysMon.batAppTotal : 0) : (SysMon.batAppCpu > 0 ? modelData.cpu / SysMon.batAppCpu : 0)
                    Layout.fillWidth: true
                    spacing: 10

                    Text {
                        Layout.preferredWidth: 130
                        text: appRow.modelData.name
                        color: Colors.text
                        font.pixelSize: 11
                        font.family: Fonts.family
                        elide: Text.ElideRight
                    }
                    Bar {
                        Layout.fillWidth: true
                        value: appRow.share
                        accent: Colors.textDimmed
                    }
                    Text {
                        Layout.preferredWidth: 38
                        text: `${(appRow.share * 100).toFixed(0)}%`
                        color: Colors.textBright
                        font.pixelSize: 11
                        font.family: Fonts.family
                        horizontalAlignment: Text.AlignRight
                    }
                    Text {
                        Layout.preferredWidth: 60
                        text: SysMon.batOnBattery ? (appRow.modelData.wh >= 1 ? `${appRow.modelData.wh.toFixed(2)} Wh` : `${(appRow.modelData.wh * 1000).toFixed(0)} mWh`) : (appRow.modelData.cpu < 60 ? `${Math.round(appRow.modelData.cpu)}s` : SysMon.fmtDuration(appRow.modelData.cpu))
                        color: Colors.textMuted
                        font.pixelSize: 10
                        font.family: Fonts.family
                        horizontalAlignment: Text.AlignRight
                    }
                }
            }
        }

        Panel {
            Layout.fillWidth: true
            Layout.fillHeight: true
            Layout.preferredWidth: 1

            GridLayout {
                Layout.fillWidth: true
                columns: 3
                rowSpacing: 14
                columnSpacing: 12

                Stat { Layout.fillWidth: true; label: "Health"; value: SysMon.batHealth > 0 ? `${SysMon.batHealth.toFixed(0)}%` : "—" }
                Stat { Layout.fillWidth: true; label: "Full capacity"; value: `${SysMon.batEnergyFull.toFixed(1)} Wh` }
                Stat { Layout.fillWidth: true; label: "Design capacity"; value: `${SysMon.batEnergyDesign.toFixed(1)} Wh` }
                Stat { Layout.fillWidth: true; label: "Cycles"; value: SysMon.batCycles > 0 ? String(SysMon.batCycles) : "Not reported" }
                Stat { Layout.fillWidth: true; label: "Charge limit"; value: SysMon.batThreshold > 0 ? `${SysMon.batThreshold}%` : "—" }
                Stat { Layout.fillWidth: true; label: "Voltage"; value: `${SysMon.batVoltage.toFixed(2)} V` }
                Stat { Layout.fillWidth: true; label: "Model"; value: [SysMon.batVendor, SysMon.batModel].filter(s => s.length > 0).join(" ") || "—" }
                Stat { Layout.fillWidth: true; label: "Chemistry"; value: SysMon.batTech.length > 0 ? SysMon.batTech : "—" }
                Stat { Layout.fillWidth: true; label: "Adapter"; value: SysMon.acOnline ? "Connected" : "Disconnected" }
            }
        }
    }

    RowLayout {
        Layout.fillWidth: true
        Layout.fillHeight: false
        visible: Asusctl.isAvailable
        spacing: 12

        Text {
            text: "Power profile"
            color: Colors.textMuted
            font.pixelSize: 11
            font.family: Fonts.family
        }

        Item { Layout.fillWidth: true }

        Segmented {
            Layout.preferredWidth: 300
            implicitHeight: 30
            items: root.profiles
            currentIndex: Math.max(0, root.profiles.indexOf(Asusctl.activeProfile))
            onSelected: i => Asusctl.setProfile(root.profiles[i])
        }
    }
}
