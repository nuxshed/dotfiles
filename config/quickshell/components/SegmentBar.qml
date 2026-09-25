pragma ComponentBehavior: Bound

import QtQuick
import "../config"

Item {
    id: root

    property var segments: []
    property real from: 0
    property real to: 1
    property real gap: 1
    property real now: Date.now()

    readonly property real span: Math.max(1, to - from)

    function tint(kind: string): color {
        return kind === "focus" ? Colors.primary : kind === "break" ? Colors.green : Colors.outline;
    }

    implicitHeight: 6

    Rectangle {
        anchors.fill: parent
        radius: height / 2
        color: Colors.surfaceActive
    }

    Item {
        anchors.fill: parent
        clip: true

        Repeater {
            model: root.segments

            Rectangle {
                id: seg

                required property var modelData

                readonly property real a: Math.max(root.from, modelData.start)
                readonly property real b: Math.min(root.to, modelData.end ?? root.now)

                visible: b > a
                x: (a - root.from) / root.span * root.width
                width: Math.max(1.5, (b - a) / root.span * root.width - root.gap)
                height: root.height
                radius: Math.min(height / 2, width / 2, 4)
                color: root.tint(modelData.kind)
            }
        }
    }
}
