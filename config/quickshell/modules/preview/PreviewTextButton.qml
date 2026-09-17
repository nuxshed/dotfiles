import QtQuick
import "../../config"

Rectangle {
    id: root

    property string text: ""
    property bool primary: false

    signal clicked

    implicitWidth: label.width + 28
    implicitHeight: 32
    radius: height / 2
    color: root.primary ? (!root.enabled ? Colors.surfaceActive : mouse.containsMouse ? Qt.lighter(Colors.blue, 1.1) : Colors.blue)
        : mouse.containsMouse ? Colors.subtle : "transparent"
    opacity: root.enabled ? 1 : 0.5

    Behavior on color { ColorAnimation { duration: 140 } }

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
