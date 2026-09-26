import QtQuick
import Quickshell.Widgets
import "../../components"
import "../../config"

Item {
    id: root

    property real value: 0
    property string icon: ""
    property bool muted: false
    property bool shown: false
    property real pending: 0
    readonly property bool dragging: drag.pressed
    readonly property real level: !shown ? 0 : dragging ? pending : Math.min(1, value)
    property real fill: level

    signal moved(real value)
    signal iconClicked

    implicitHeight: 140

    Behavior on fill {
        enabled: !root.dragging

        NumberAnimation {
            duration: 460
            easing.type: Easing.OutCubic
        }
    }

    function valueAt(y: real): real {
        return Math.max(0, Math.min(1, 1 - y / height));
    }

    ClippingRectangle {
        anchors.fill: parent
        radius: 22
        color: Colors.surface

        Rectangle {
            anchors.bottom: parent.bottom
            width: parent.width
            height: parent.height * root.fill
            color: root.muted ? Colors.subtle : Colors.text

            Behavior on color {
                ColorAnimation { duration: 150 }
            }
        }

        Text {
            anchors.horizontalCenter: parent.horizontalCenter
            y: 14
            text: Math.round(root.level * 100)
            color: root.fill > 0.86 && !root.muted ? Colors.background : Colors.textDimmed
            font.pixelSize: 12
            font.family: Fonts.family
            font.weight: Font.Medium
        }

        MaterialIcon {
            anchors.horizontalCenter: parent.horizontalCenter
            anchors.bottom: parent.bottom
            anchors.bottomMargin: 12
            text: root.icon
            size: 20
            color: root.fill * root.height > 34 && !root.muted ? Colors.background : Colors.textMuted
        }
    }

    MouseArea {
        id: drag

        anchors.fill: parent
        anchors.bottomMargin: 44
        cursorShape: Qt.PointingHandCursor
        preventStealing: true
        onPressed: event => {
            root.pending = root.valueAt(event.y);
            root.moved(root.pending);
        }
        onPositionChanged: event => {
            root.pending = root.valueAt(event.y);
            root.moved(root.pending);
        }
        onWheel: event => root.moved(Math.max(0, Math.min(1, root.value + (event.angleDelta.y > 0 ? 0.05 : -0.05))))
    }

    MouseArea {
        anchors.fill: parent
        anchors.topMargin: parent.height - 44
        cursorShape: Qt.PointingHandCursor
        onClicked: root.iconClicked()
        onWheel: event => root.moved(Math.max(0, Math.min(1, root.value + (event.angleDelta.y > 0 ? 0.05 : -0.05))))
    }
}
