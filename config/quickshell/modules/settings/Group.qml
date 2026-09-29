import QtQuick
import QtQuick.Layouts
import "../../config"

ColumnLayout {
    id: root

    property string title: ""
    property int padding: 0
    default property alias rows: box.data

    Layout.fillWidth: true
    spacing: 8

    Text {
        Layout.leftMargin: 4
        text: root.title
        color: Colors.textMuted
        font.pixelSize: 12
        font.family: Fonts.family
        font.weight: Font.Medium
        visible: text.length > 0
    }

    Rectangle {
        Layout.fillWidth: true
        implicitHeight: box.implicitHeight + root.padding * 2
        radius: 12
        color: Colors.surfaceActive

        Column {
            id: box

            x: root.padding
            y: root.padding
            width: parent.width - root.padding * 2
        }
    }
}
