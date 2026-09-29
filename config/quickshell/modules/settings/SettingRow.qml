import QtQuick
import QtQuick.Layouts
import "../../components"
import "../../config"

Item {
    id: root

    property string icon: ""
    property string label: ""
    property string description: ""
    property bool clickable: false
    property bool selected: false
    default property alias control: slot.data

    signal clicked

    width: parent ? parent.width : 0
    implicitHeight: Math.max(54, texts.implicitHeight + 24)

    Rectangle {
        x: 16
        width: parent.width - 32
        height: 1
        color: Colors.subtle
        visible: !root.Positioner.isFirstItem
    }

    Rectangle {
        anchors.fill: parent
        anchors.margins: 4
        radius: 9
        color: Colors.subtle
        opacity: root.clickable && area.containsMouse ? 0.6 : 0

        Behavior on opacity {
            NumberAnimation { duration: 120 }
        }
    }

    RowLayout {
        anchors.fill: parent
        anchors.leftMargin: 16
        anchors.rightMargin: 16
        spacing: 14

        MaterialIcon {
            visible: root.icon.length > 0
            text: root.icon
            size: 18
            color: root.selected ? Colors.primary : Colors.textDimmed
        }

        ColumnLayout {
            id: texts

            Layout.fillWidth: true
            spacing: 2

            Text {
                Layout.fillWidth: true
                text: root.label
                color: root.selected ? Colors.textBright : Colors.text
                font.pixelSize: 13
                font.family: Fonts.family
                font.weight: root.selected ? Font.Medium : Font.Normal
                elide: Text.ElideRight
            }

            Text {
                Layout.fillWidth: true
                text: root.description
                color: Colors.textMuted
                font.pixelSize: 11
                font.family: Fonts.family
                wrapMode: Text.Wrap
                maximumLineCount: 2
                elide: Text.ElideRight
                visible: text.length > 0
            }
        }

        Row {
            id: slot
            spacing: 8
        }

        MaterialIcon {
            visible: root.selected
            text: "check"
            size: 16
            color: Colors.primary
        }
    }

    MouseArea {
        id: area
        anchors.fill: parent
        enabled: root.clickable
        hoverEnabled: true
        cursorShape: Qt.PointingHandCursor
        onClicked: root.clicked()
    }
}
