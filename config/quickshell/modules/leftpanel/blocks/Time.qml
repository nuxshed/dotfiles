import Quickshell
import QtQuick
import "../../../config"

Item {
    width: 44
    height: 64
    Rectangle {
        anchors.fill: parent
        radius: 6
        color: Colors.surface
        Column {
            anchors.centerIn: parent
            spacing: 1
            Text {
                id: hoursText
                text: Qt.formatDateTime(new Date(), Settings.clock24 ? "hh" : "hh ap").slice(0, 2)
                color: Colors.textBright
                font.pixelSize: 14
                font.family: Fonts.family
                font.weight: Font.Bold
                horizontalAlignment: Text.AlignHCenter
            }
            Text {
                id: minutesText
                text: Qt.formatDateTime(new Date(), "mm")
                color: Colors.textDimmed
                font.pixelSize: 14
                font.family: Fonts.family
                font.weight: Font.Bold
                horizontalAlignment: Text.AlignHCenter
            }
        }
    }
    property bool isHovered: false
    signal clicked()

    MouseArea {
        anchors.fill: parent
        cursorShape: Qt.PointingHandCursor
        hoverEnabled: true // Enable hover
        onEntered: {
            parent.isHovered = true
            parent.clicked()
        }
        onExited: {
            parent.isHovered = false
        }
    }

    Timer {
        interval: 1000
        running: true
        repeat: true
        onTriggered: {
            hoursText.text = Qt.formatDateTime(new Date(), Settings.clock24 ? "hh" : "hh ap").slice(0, 2)
            minutesText.text = Qt.formatDateTime(new Date(), "mm")
        }
    }
}
