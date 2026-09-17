import QtQuick
import "../../config"

Column {
    property string label: ""
    property string value: ""

    spacing: 3

    Text {
        text: parent.label.toUpperCase()
        color: Colors.textMuted
        font.pixelSize: 10
        font.family: Fonts.family
        font.letterSpacing: 0.5
    }
    Text {
        text: parent.value
        color: Colors.textBright
        font.pixelSize: 14
        font.family: Fonts.family
        font.weight: Font.Medium
    }
}
