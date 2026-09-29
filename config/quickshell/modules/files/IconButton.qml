import QtQuick
import "../../components"
import "../../config"

Rectangle {
    id: root

    property string icon: ""
    property bool active: false

    signal clicked

    implicitWidth: 32
    implicitHeight: 32
    radius: height / 2
    color: mouse.containsMouse && root.enabled ? Colors.surfaceActive : root.active ? Colors.surface : "transparent"
    opacity: root.enabled ? 1 : 0.35

    Behavior on color {
        ColorAnimation { duration: 120 }
    }

    MaterialIcon {
        anchors.centerIn: parent
        text: root.icon
        size: 18
        color: root.active ? Colors.primary : mouse.containsMouse ? Colors.textBright : Colors.textDimmed
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
