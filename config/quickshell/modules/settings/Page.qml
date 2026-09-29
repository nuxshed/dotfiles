import QtQuick
import QtQuick.Layouts
import "../../config"

Flickable {
    id: root

    property string title: ""
    property string subtitle: ""
    default property alias content: column.data

    contentHeight: column.implicitHeight + 56
    clip: true
    boundsBehavior: Flickable.StopAtBounds

    ColumnLayout {
        id: column

        x: 28
        y: 26
        width: root.width - 56
        spacing: 24

        Column {
            Layout.fillWidth: true
            spacing: 4

            Text {
                text: root.title
                color: Colors.textBright
                font.pixelSize: 20
                font.family: Fonts.family
                font.weight: Font.Medium
            }

            Text {
                width: parent.width
                text: root.subtitle
                color: Colors.textMuted
                font.pixelSize: 12
                font.family: Fonts.family
                wrapMode: Text.Wrap
                visible: text.length > 0
            }
        }
    }
}
