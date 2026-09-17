pragma ComponentBehavior: Bound

import QtQuick
import "../../components"
import "../../config"

Rectangle {
    id: root

    property var items: []
    property int currentIndex: 0

    signal selected(int index)

    readonly property real segmentWidth: (width - 8) / Math.max(1, items.length)

    implicitHeight: 34
    radius: 10
    color: Colors.surfaceActive
    border.width: 1
    border.color: Colors.outline

    Rectangle {
        x: 4 + root.currentIndex * root.segmentWidth
        y: 4
        width: root.segmentWidth
        height: parent.height - 8
        radius: 7
        color: Colors.primary

        Behavior on x {
            NumberAnimation { duration: 240; easing.type: Easing.OutCubic }
        }
    }

    Row {
        anchors.fill: parent
        anchors.margins: 4

        Repeater {
            model: root.items

            Item {
                id: segment

                required property int index
                required property var modelData

                readonly property bool active: root.currentIndex === segment.index

                width: root.segmentWidth
                height: parent.height

                Row {
                    anchors.centerIn: parent
                    spacing: 7

                    MaterialIcon {
                        text: segment.modelData.icon
                        size: 14
                        color: segment.active ? Colors.primaryText : Colors.textMuted
                        anchors.verticalCenter: parent.verticalCenter
                    }
                    Text {
                        text: segment.modelData.name
                        color: segment.active ? Colors.primaryText : hover.containsMouse ? Colors.text : Colors.textDimmed
                        font.pixelSize: 12
                        font.family: Fonts.family
                        font.weight: segment.active ? Font.Medium : Font.Normal
                        anchors.verticalCenter: parent.verticalCenter
                    }
                }

                MouseArea {
                    id: hover
                    anchors.fill: parent
                    hoverEnabled: true
                    cursorShape: Qt.PointingHandCursor
                    onClicked: root.selected(segment.index)
                }
            }
        }
    }
}
