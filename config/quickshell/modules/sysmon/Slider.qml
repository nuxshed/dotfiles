import QtQuick
import "../../config"

Item {
    id: root

    property real from: 0
    property real to: 100
    property real step: 1
    property real value: 0
    property real pending: value
    property color accent: Colors.textBright
    readonly property bool dragging: mouse.pressed

    signal committed(real value)

    implicitHeight: 22

    function valueAt(x: real): real {
        const f = Math.max(0, Math.min(1, (x - 8) / (width - 16)));
        const v = root.from + f * (root.to - root.from);
        return Math.round(v / root.step) * root.step;
    }

    onValueChanged: if (!mouse.pressed) root.pending = value

    Rectangle {
        anchors.verticalCenter: parent.verticalCenter
        x: 8
        width: parent.width - 16
        height: 6
        radius: 3
        color: Colors.subtle

        Rectangle {
            width: (root.pending - root.from) / (root.to - root.from) * parent.width
            height: parent.height
            radius: 3
            color: root.accent
        }
    }

    Rectangle {
        x: 8 + (root.pending - root.from) / (root.to - root.from) * (parent.width - 16) - width / 2
        anchors.verticalCenter: parent.verticalCenter
        width: 16
        height: 16
        radius: 8
        color: mouse.pressed || mouse.containsMouse ? Colors.textBright : Colors.text
        border.width: 3
        border.color: Colors.background
    }

    MouseArea {
        id: mouse
        anchors.fill: parent
        hoverEnabled: true
        cursorShape: Qt.PointingHandCursor
        onPressed: mouse => root.pending = root.valueAt(mouse.x)
        onPositionChanged: mouse => { if (pressed) root.pending = root.valueAt(mouse.x); }
        onReleased: {
            if (root.pending !== root.value)
                root.committed(root.pending);
        }
    }
}
