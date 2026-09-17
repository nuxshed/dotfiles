import QtQuick
import "../../config"

Item {
    id: root

    required property real unit

    property date now: new Date()

    readonly property real elapsed: now.getHours() * 3600000 + now.getMinutes() * 60000 + now.getSeconds() * 1000 + now.getMilliseconds()
    readonly property real cx: 40 * unit
    readonly property real cy: height / 2

    Timer {
        interval: 16
        running: true
        repeat: true
        onTriggered: root.now = new Date()
    }

    Rectangle {
        id: pill

        x: root.cx + 230 * root.unit
        anchors.verticalCenter: parent.verticalCenter
        width: 330 * root.unit
        height: 90 * root.unit
        radius: height / 2
        color: "transparent"
        border.color: Colors.subtle
        border.width: 1

        Rectangle {
            x: 170 * root.unit
            anchors.verticalCenter: parent.verticalCenter
            width: 1
            height: 35 * root.unit
            color: Colors.subtle
        }
    }

    ClockRing {
        cx: root.cx
        cy: root.cy
        radius: 320 * root.unit
        angle: -(root.elapsed % 3600000) / 3600000 * 360
        tick: 18 * root.unit
        label: 22 * root.unit
    }

    ClockRing {
        cx: root.cx
        cy: root.cy
        radius: 480 * root.unit
        angle: -(root.elapsed % 60000) / 60000 * 360
        tick: 13 * root.unit
        label: 16 * root.unit
    }

    Text {
        anchors.right: pill.left
        anchors.rightMargin: 40 * root.unit
        anchors.verticalCenter: parent.verticalCenter
        text: String(root.now.getHours()).padStart(2, "0")
        font.pixelSize: 110 * root.unit
        font.family: Fonts.family
        font.weight: Font.Black
        color: Colors.textBright
    }

    Column {
        anchors.left: pill.right
        anchors.leftMargin: 110 * root.unit
        anchors.verticalCenter: parent.verticalCenter
        spacing: 5 * root.unit

        Text {
            text: Qt.formatDate(root.now, "dd MMM yyyy").toUpperCase()
            font.pixelSize: 13 * root.unit
            font.family: Fonts.family
            font.letterSpacing: 4 * root.unit
            color: Colors.textMuted
        }

        Text {
            text: Qt.formatDate(root.now, "dddd").toUpperCase()
            font.pixelSize: 18 * root.unit
            font.family: Fonts.family
            font.letterSpacing: 8 * root.unit
            font.weight: Font.Medium
            color: Colors.textBright
        }
    }
}
