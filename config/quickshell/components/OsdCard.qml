import QtQuick
import QtQuick.Layouts
import Quickshell
import "../config"

PanelWindow {
    id: root

    property string icon: ""
    property real value: 0
    property bool muted: false

    anchors.bottom: true
    margins.bottom: screen.height / 13
    exclusiveZone: 0
    implicitWidth: 300
    implicitHeight: 64
    color: "transparent"

    mask: Region {}

    Rectangle {
        anchors.centerIn: parent
        width: parent.width - 40
        height: 44
        radius: height / 2
        color: Colors.background
        border.color: Colors.border
        border.width: 1

        RowLayout {
            anchors.fill: parent
            anchors.leftMargin: 16
            anchors.rightMargin: 18
            spacing: 12

            MaterialIcon {
                text: root.icon
                size: 18
                color: root.muted ? Colors.textMuted : Colors.textBright
            }

            Rectangle {
                Layout.fillWidth: true
                implicitHeight: 6
                radius: 3
                color: Colors.subtle

                Rectangle {
                    width: parent.width * Math.min(root.value, 1)
                    height: parent.height
                    radius: parent.radius
                    color: root.muted ? Colors.textMuted : Colors.text

                    Behavior on width {
                        NumberAnimation { duration: 90 }
                    }
                }

                Rectangle {
                    width: parent.width * Math.max(root.value - 1, 0)
                    height: parent.height
                    radius: parent.radius
                    color: Colors.red
                    visible: root.value > 1
                }
            }

            Text {
                Layout.preferredWidth: 34
                horizontalAlignment: Text.AlignRight
                text: Math.round(root.value * 100)
                color: Colors.text
                font.pixelSize: 13
                font.family: Fonts.family
                font.weight: Font.Medium
                font.features: { "tnum": 1 }
            }
        }
    }
}
