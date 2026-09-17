pragma ComponentBehavior: Bound

import QtQuick
import "../../config"

Repeater {
    id: root

    required property real cx
    required property real cy
    required property real radius
    required property real angle
    required property real tick
    required property real label

    model: 60

    delegate: Item {
        id: mark

        required property int index

        readonly property bool major: index % 5 === 0
        readonly property real rot: index * 6 + root.angle
        readonly property real rad: mark.rot * Math.PI / 180
        readonly property real offset: {
            let a = mark.rot % 360;
            if (a > 180)
                a -= 360;
            if (a < -180)
                a += 360;
            return a;
        }
        readonly property real glow: Math.max(0, 1 - Math.abs(mark.offset) / 4)

        Rectangle {
            x: root.cx + root.radius * Math.cos(mark.rad) - width / 2
            y: root.cy + root.radius * Math.sin(mark.rad) - height / 2
            width: mark.major ? 2 : 1
            height: mark.major ? root.tick : root.tick * 0.6
            rotation: mark.rot + 90
            color: Qt.rgba(1, 1, 1, mark.glow > 0 ? 1 : mark.major ? 0.3 : 0.15)
        }

        Text {
            readonly property real inner: root.radius - root.label * 1.8

            visible: mark.major
            x: root.cx + inner * Math.cos(mark.rad) - width / 2
            y: root.cy + inner * Math.sin(mark.rad) - height / 2
            rotation: mark.rot
            text: String(mark.index).padStart(2, "0")
            font.pixelSize: root.label
            font.family: Fonts.family
            font.weight: mark.glow > 0.5 ? Font.Medium : Font.Normal
            color: Qt.rgba(1, 1, 1, mark.glow > 0 ? 0.4 + mark.glow * 0.6 : 0.25)
        }
    }
}
