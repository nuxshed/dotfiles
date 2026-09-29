import QtQuick
import "../config"

Item {
    id: root

    property real value: 0
    property string icon: ""
    property bool muted: false
    readonly property bool dragging: mouse.pressed
    property real shown: value
    property color track: Colors.surface

    signal moved(real value)
    signal iconClicked

    implicitHeight: 32

    function valueAt(x: real): real {
        const lead = root.icon ? height : 0;
        return Math.max(0, Math.min(1, (x - lead / 2) / (width - lead / 2)));
    }

    Rectangle {
        id: track
        anchors.fill: parent
        radius: height / 2
        color: root.track

        Rectangle {
            readonly property real lead: root.icon ? root.height : 0

            height: parent.height
            width: Math.max(lead, lead / 2 + Math.min(1, root.shown) * (parent.width - lead / 2))
            radius: height / 2
            color: root.muted ? Colors.subtle : Colors.text
            visible: width > 0

            Behavior on color {
                ColorAnimation { duration: 150 }
            }
        }

        MaterialIcon {
            visible: root.icon !== ""
            width: root.height
            height: root.height
            text: root.icon
            size: 17
            color: root.muted ? Colors.textMuted : Colors.background
        }
    }

    MouseArea {
        id: mouse
        anchors.fill: parent
        anchors.leftMargin: root.icon ? root.height : 0
        anchors.topMargin: -6
        anchors.bottomMargin: -6
        cursorShape: Qt.PointingHandCursor
        preventStealing: true
        onPressed: event => {
            root.shown = root.valueAt(event.x + anchors.leftMargin);
            root.moved(root.shown);
        }
        onPositionChanged: event => {
            root.shown = root.valueAt(event.x + anchors.leftMargin);
            root.moved(root.shown);
        }
        onReleased: root.shown = Qt.binding(() => root.value)
        onWheel: event => root.moved(Math.max(0, Math.min(1, root.value + (event.angleDelta.y > 0 ? 0.05 : -0.05))))
    }

    MouseArea {
        visible: root.icon !== ""
        width: root.height
        height: root.height
        cursorShape: Qt.PointingHandCursor
        onClicked: root.iconClicked()
    }
}
