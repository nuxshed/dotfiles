import QtQuick
import "../config"

Rectangle {
    id: root

    property var items: []
    property int currentIndex: 0

    signal selected(int index)

    readonly property real segmentWidth: (width - 8) / Math.max(1, items.length)

    implicitHeight: 38
    radius: height / 2
    color: Colors.surface

    Rectangle {
        x: 4 + root.currentIndex * root.segmentWidth
        y: 4
        width: root.segmentWidth
        height: parent.height - 8
        radius: height / 2
        color: Colors.surfaceActive

        Behavior on x {
            NumberAnimation { duration: 280; easing.type: Easing.OutCubic }
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
                readonly property string label: segment.modelData.name ?? segment.modelData
                readonly property color tint: segment.modelData.tint ?? Colors.textBright

                width: root.segmentWidth
                height: parent.height

                Text {
                    anchors.centerIn: parent
                    text: segment.label
                    color: segment.active ? segment.tint
                        : segHover.hovered ? Colors.text : Colors.textMuted
                    font.pixelSize: 11
                    font.bold: segment.active

                    Behavior on color {
                        ColorAnimation { duration: 150 }
                    }
                }

                HoverHandler {
                    id: segHover
                    cursorShape: Qt.PointingHandCursor
                }

                TapHandler {
                    onTapped: root.selected(segment.index)
                }
            }
        }
    }
}
