pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import Quickshell
import "../../components"
import "../../config"
import "../../services"

Item {
    id: root

    readonly property var i: SysMon.info
    readonly property string logo: Quickshell.iconPath("nix-snowflake", true)
    readonly property var facts: [
        { label: "Kernel", value: i.kernel ? `Linux ${i.kernel}` : "" },
        { label: "Uptime", value: SysMon.fmtDuration(SysMon.uptime) },
        { label: "Packages", value: i.pkgs_system ? `${i.pkgs_system} nix` : "" },
        { label: "Shell", value: i.shell },
        { label: "Compositor", value: i.wm },
        { label: "Display", value: SysMon.displays.length > 0 ? SysMon.displays[0].replace(/^\S+ /, "").replace(/, scale.*$/, "") : "" },
        { label: "CPU", value: SysMon.cpuModel },
        { label: "GPU", value: SysMon.gpus.length > 0 ? SysMon.gpus[SysMon.gpus.length - 1].replace(/^NVIDIA \S+ /, "").replace(/ \/ Mobile$/, "") : "" },
        { label: "Memory", value: `${SysMon.fmtBytes(SysMon.memUsed, 1)} of ${SysMon.fmtBytes(SysMon.memTotal, 1)}` },
        { label: "Storage", value: SysMon.rootSize > 0 ? `${SysMon.fmtBytes(SysMon.rootSize - SysMon.rootUsed, 0)} free of ${SysMon.fmtBytes(SysMon.rootSize, 0)}` : "" }
    ]

    Component.onCompleted: SysMon.refreshInfo()

    ColumnLayout {
        anchors.centerIn: parent
        width: Math.min(parent.width, 560)
        spacing: 24

        Image {
            Layout.alignment: Qt.AlignHCenter
            Layout.preferredWidth: 140
            Layout.preferredHeight: 140
            source: root.logo
            sourceSize: Qt.size(280, 280)
            smooth: true
            mipmap: true
            visible: status === Image.Ready
        }

        MaterialIcon {
            Layout.alignment: Qt.AlignHCenter
            visible: root.logo.length === 0
            text: "computer"
            size: 96
        }

        Column {
            Layout.alignment: Qt.AlignHCenter
            Layout.topMargin: -6
            spacing: 4

            Text {
                anchors.horizontalCenter: parent.horizontalCenter
                text: root.i.os ?? "Linux"
                color: Colors.textBright
                font.pixelSize: 24
                font.family: Fonts.family
                font.weight: Font.Medium
            }
            Text {
                anchors.horizontalCenter: parent.horizontalCenter
                text: (root.i.host ?? "") + "  ·  " + Quickshell.env("USER") + "@" + SysMon.hostname
                color: Colors.textMuted
                font.pixelSize: 12
                font.family: Fonts.family
            }
        }

        Rectangle {
            Layout.alignment: Qt.AlignHCenter
            width: 40
            height: 1
            color: Colors.outline
        }

        GridLayout {
            Layout.alignment: Qt.AlignHCenter
            columns: 2
            rowSpacing: 9
            columnSpacing: 14

            Repeater {
                model: root.facts.filter(f => f.value && f.value.length > 0)

                Text {
                    required property var modelData
                    required property int index
                    Layout.row: index
                    Layout.column: 0
                    Layout.preferredWidth: 110
                    text: modelData.label
                    color: Colors.textMuted
                    font.pixelSize: 12
                    font.family: Fonts.family
                    horizontalAlignment: Text.AlignRight
                }
            }

            Repeater {
                model: root.facts.filter(f => f.value && f.value.length > 0)

                Text {
                    required property var modelData
                    required property int index
                    Layout.row: index
                    Layout.column: 1
                    Layout.preferredWidth: 300
                    text: modelData.value
                    color: Colors.textBright
                    font.pixelSize: 12
                    font.family: Fonts.family
                    elide: Text.ElideRight
                }
            }
        }

        Row {
            Layout.alignment: Qt.AlignHCenter
            Layout.topMargin: 6
            spacing: 8

            Repeater {
                model: [Colors.red, Colors.orange, Colors.yellow, Colors.green, Colors.cyan, Colors.blue, Colors.magenta, Colors.textBright]

                Rectangle {
                    required property var modelData
                    width: 14
                    height: 14
                    radius: 7
                    color: modelData
                }
            }
        }
    }
}
