import QtQuick
import "../../components"
import "../../config"

Item {
    id: root

    property real level: 0
    property real limit: 100
    property color accent: Colors.green
    property bool charging: false

    implicitWidth: 200
    implicitHeight: 88

    Rectangle {
        id: body
        anchors.fill: parent
        anchors.rightMargin: 10
        radius: 14
        color: Colors.background
        border.width: 3
        border.color: Colors.outline

        Rectangle {
            anchors.left: parent.left
            anchors.top: parent.top
            anchors.bottom: parent.bottom
            anchors.margins: 7
            width: Math.max(0, (parent.width - 14) * Math.min(1, root.level / 100))
            radius: 8
            color: root.accent

            Behavior on width { NumberAnimation { duration: 600; easing.type: Easing.OutCubic } }
        }

        Rectangle {
            visible: root.limit > 0 && root.limit < 100
            x: 7 + (parent.width - 14) * root.limit / 100 - 1
            y: 4
            width: 2
            height: parent.height - 8
            color: Colors.textMuted
        }

        Row {
            anchors.centerIn: parent
            spacing: 6

            MaterialIcon {
                visible: root.charging
                text: "flash_on"
                size: 22
                color: Colors.textBright
                anchors.verticalCenter: parent.verticalCenter
            }
            Text {
                text: `${Math.round(root.level)}%`
                color: Colors.textBright
                font.pixelSize: 26
                font.family: Fonts.family
                font.weight: Font.Medium
                anchors.verticalCenter: parent.verticalCenter
                style: Text.Outline
                styleColor: Qt.alpha(Colors.background, 0.5)
            }
        }
    }

    Rectangle {
        anchors.left: body.right
        anchors.leftMargin: 1
        anchors.verticalCenter: parent.verticalCenter
        width: 9
        height: parent.height * 0.4
        radius: 3
        color: Colors.outline
    }
}
