import QtQuick
import QtQuick.Layouts
import "../../config"

RowLayout {
    property string label: ""
    property string value: ""

    Layout.fillWidth: true
    spacing: 12

    Text {
        Layout.fillWidth: true
        text: parent.label
        color: Colors.textMuted
        font.pixelSize: 12
        font.family: Fonts.family
        elide: Text.ElideRight
    }
    Text {
        text: parent.value
        color: Colors.text
        font.pixelSize: 12
        font.family: Fonts.family
        horizontalAlignment: Text.AlignRight
    }
}
