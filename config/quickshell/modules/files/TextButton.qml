import QtQuick
import "../../config"

Rectangle {
    id: root

    property string text: ""
    property bool primary: false
    property bool danger: false

    signal clicked

    implicitWidth: label.width + (root.primary ? 36 : 28)
    implicitHeight: 36
    radius: height / 2
    color: {
        if (root.primary) {
            const base = root.danger ? Colors.red : Colors.blue;
            return !root.enabled ? Colors.surface : mouse.containsMouse ? Qt.lighter(base, 1.1) : base;
        }
        return mouse.containsMouse ? Colors.surface : "transparent";
    }
    opacity: root.enabled ? 1 : 0.6

    Behavior on color {
        ColorAnimation { duration: 150 }
    }

    Text {
        id: label
        anchors.centerIn: parent
        text: root.text
        color: root.primary ? (root.enabled ? Colors.background : Colors.textMuted) : Colors.text
        font.pixelSize: 12
        font.family: Fonts.family
        font.weight: root.primary ? Font.Medium : Font.Normal
    }

    MouseArea {
        id: mouse
        anchors.fill: parent
        enabled: root.enabled
        hoverEnabled: true
        cursorShape: Qt.PointingHandCursor
        onClicked: root.clicked()
    }
}
