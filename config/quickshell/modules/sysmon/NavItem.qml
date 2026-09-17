import QtQuick
import "../../components"
import "../../config"

Rectangle {
    id: root

    property string icon: ""
    property string text: ""
    property bool active: false

    signal clicked

    implicitHeight: 38
    radius: 10
    color: root.active ? Colors.primaryContainer : mouse.containsMouse ? Colors.surface : "transparent"

    Behavior on color { ColorAnimation { duration: 120 } }

    Row {
        anchors.verticalCenter: parent.verticalCenter
        x: 14
        spacing: 12

        MaterialIcon {
            text: root.icon
            size: 16
            color: root.active ? Colors.primaryContainerText : Colors.textMuted
            anchors.verticalCenter: parent.verticalCenter
        }
        Text {
            text: root.text
            color: root.active ? Colors.primaryContainerText : Colors.textDimmed
            font.pixelSize: 13
            font.family: Fonts.family
            font.weight: root.active ? Font.Medium : Font.Normal
            anchors.verticalCenter: parent.verticalCenter
        }
    }

    MouseArea {
        id: mouse
        anchors.fill: parent
        hoverEnabled: true
        cursorShape: Qt.PointingHandCursor
        onClicked: root.clicked()
    }
}
