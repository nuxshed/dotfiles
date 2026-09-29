import QtQuick
import "../../components"
import "../../config"

Rectangle {
    id: root

    property string text: ""
    property string icon: ""
    property bool accent: false

    signal clicked

    implicitWidth: row.implicitWidth + 28
    implicitHeight: 30
    radius: height / 2
    color: accent ? (area.containsMouse ? Qt.lighter(Colors.primary, 1.08) : Colors.primary) : area.containsMouse ? Colors.outline : Colors.subtle

    Behavior on color {
        ColorAnimation { duration: 120 }
    }

    Row {
        id: row

        anchors.centerIn: parent
        spacing: 6

        MaterialIcon {
            anchors.verticalCenter: parent.verticalCenter
            visible: root.icon.length > 0
            text: root.icon
            size: 15
            color: root.accent ? Colors.primaryText : Colors.textBright
        }

        Text {
            anchors.verticalCenter: parent.verticalCenter
            text: root.text
            color: root.accent ? Colors.primaryText : Colors.textBright
            font.pixelSize: 12
            font.family: Fonts.family
            font.weight: Font.Medium
            visible: text.length > 0
        }
    }

    MouseArea {
        id: area
        anchors.fill: parent
        hoverEnabled: true
        cursorShape: Qt.PointingHandCursor
        onClicked: root.clicked()
    }
}
