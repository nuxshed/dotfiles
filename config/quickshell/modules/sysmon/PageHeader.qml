import QtQuick
import QtQuick.Layouts
import "../../config"

RowLayout {
    property string title: ""
    property string subtitle: ""
    property string value: ""
    property string valueLabel: ""
    property color valueColor: Colors.textBright

    Layout.fillWidth: true
    Layout.fillHeight: false
    spacing: 12

    Column {
        Layout.fillWidth: true
        spacing: 3

        Text {
            text: parent.parent.title
            color: Colors.textBright
            font.pixelSize: 18
            font.family: Fonts.family
            font.weight: Font.Medium
        }
        Text {
            text: parent.parent.subtitle
            color: Colors.textMuted
            font.pixelSize: 12
            font.family: Fonts.family
            visible: text.length > 0
        }
    }

    Column {
        spacing: 1
        visible: parent.value.length > 0

        Text {
            anchors.right: parent.right
            text: parent.parent.value
            color: parent.parent.valueColor
            font.pixelSize: 24
            font.family: Fonts.family
            font.weight: Font.Medium
        }
        Text {
            anchors.right: parent.right
            text: parent.parent.valueLabel
            color: Colors.textMuted
            font.pixelSize: 10
            font.family: Fonts.family
            visible: text.length > 0
        }
    }
}
