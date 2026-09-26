import QtQuick
import QtQuick.Layouts
import Quickshell.Services.Pipewire
import "../../components"
import "../../config"
import "../../services"

Rectangle {
    id: root

    required property PwNode node
    required property bool active

    implicitHeight: 36
    radius: 12
    color: active ? Colors.surfaceActive : hover.hovered ? Colors.surface : "transparent"

    Behavior on color {
        ColorAnimation { duration: 150 }
    }

    RowLayout {
        anchors.fill: parent
        anchors.leftMargin: 12
        anchors.rightMargin: 12
        spacing: 10

        MaterialIcon {
            text: Audio.deviceIcon(root.node)
            size: 16
            color: root.active ? Colors.textBright : Colors.textMuted
        }

        Text {
            Layout.fillWidth: true
            text: Audio.name(root.node)
            color: root.active ? Colors.textBright : Colors.text
            font.pixelSize: 11
            font.family: Fonts.family
            elide: Text.ElideRight
        }

        MaterialIcon {
            visible: root.active
            text: "check"
            size: 15
            color: Colors.primary
        }
    }

    HoverHandler {
        id: hover
        cursorShape: Qt.PointingHandCursor
    }

    MouseArea {
        anchors.fill: parent
        onClicked: Audio.setDefault(root.node)
    }
}
