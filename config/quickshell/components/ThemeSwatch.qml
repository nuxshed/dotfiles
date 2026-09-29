import QtQuick
import Quickshell.Widgets
import "../config"

ClippingRectangle {
    id: root

    property string themeId: ""
    readonly property var p: Colors.paletteOf(themeId)
    readonly property real u: width / 100

    implicitWidth: 168
    implicitHeight: 104
    radius: 12
    color: p.bg

    Rectangle {
        x: 0
        y: 0
        width: 10 * root.u
        height: parent.height
        color: root.p.surface

        Column {
            anchors.horizontalCenter: parent.horizontalCenter
            y: 6 * root.u
            spacing: 2.5 * root.u

            Repeater {
                model: 3

                Rectangle {
                    required property int index
                    width: 4.5 * root.u
                    height: width
                    radius: width / 2
                    color: index === 0 ? root.p.primary : root.p.subtle
                }
            }
        }
    }

    Rectangle {
        x: 18 * root.u
        y: 12 * root.u
        width: 60 * root.u
        height: parent.height - 24 * root.u
        radius: 5 * root.u
        color: root.p.surface

        Rectangle {
            x: 7 * root.u
            y: 7 * root.u
            width: 30 * root.u
            height: 3.5 * root.u
            radius: height / 2
            color: root.p.fg
        }

        Rectangle {
            x: 7 * root.u
            y: 14 * root.u
            width: 44 * root.u
            height: 2.5 * root.u
            radius: height / 2
            color: root.p.muted
        }

        Rectangle {
            x: 7 * root.u
            y: 19.5 * root.u
            width: 36 * root.u
            height: 2.5 * root.u
            radius: height / 2
            color: root.p.muted
        }

        Rectangle {
            x: 7 * root.u
            anchors.bottom: parent.bottom
            anchors.bottomMargin: 7 * root.u
            width: 18 * root.u
            height: 7 * root.u
            radius: height / 2
            color: root.p.primary
        }
    }

    Column {
        anchors.right: parent.right
        anchors.rightMargin: 9 * root.u
        anchors.verticalCenter: parent.verticalCenter
        spacing: 2.8 * root.u

        Repeater {
            model: root.p.accents

            Rectangle {
                required property color modelData
                width: 5 * root.u
                height: width
                radius: width / 2
                color: modelData
            }
        }
    }
}
