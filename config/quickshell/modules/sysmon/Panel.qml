import QtQuick
import QtQuick.Layouts
import "../../config"

Rectangle {
    id: root

    property string title: ""
    property bool fill: false
    default property alias content: body.data

    radius: 10
    color: Colors.surfaceActive
    border.width: 1
    border.color: Colors.outline
    implicitHeight: col.implicitHeight + 28

    ColumnLayout {
        id: col
        anchors.fill: parent
        anchors.margins: 14
        spacing: 10

        Text {
            visible: root.title.length > 0
            text: root.title
            color: Colors.textMuted
            font.pixelSize: 12
            font.family: Fonts.family
            font.weight: Font.Medium
        }

        ColumnLayout {
            id: body
            Layout.fillWidth: true
            Layout.fillHeight: root.fill
            spacing: 6
        }

        Item { Layout.fillHeight: !root.fill }
    }
}
