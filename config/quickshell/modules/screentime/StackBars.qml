pragma ComponentBehavior: Bound

import QtQuick
import Quickshell.Widgets
import "../../config"
import "../../services"

Item {
    id: root

    property var bars: []
    property real max: 1
    property real spacing: 4
    property real barRadius: 3
    property string filter: ""
    property int current: -1
    property bool interactive: false
    property int hovered: -1

    signal clicked(int index)

    readonly property real barWidth: (width - spacing * Math.max(0, bars.length - 1)) / Math.max(1, bars.length)

    function barX(index: int): real {
        return index * (root.barWidth + root.spacing);
    }

    Repeater {
        model: root.bars

        Item {
            id: bar

            required property int index
            required property var modelData

            readonly property var stack: ScreenTime.categoryList.filter(c => (modelData.cats[c.id] ?? 0) > 0).map(c => ({ color: c.color, id: c.id, seconds: modelData.cats[c.id] }))
            readonly property real total: stack.reduce((s, c) => s + c.seconds, 0)
            readonly property bool lit: root.hovered === index || root.current === index

            x: root.barX(index)
            width: root.barWidth
            height: root.height

            Rectangle {
                visible: bar.total === 0
                anchors.bottom: parent.bottom
                width: parent.width
                height: 2
                radius: 1
                color: Colors.surfaceActive
            }

            ClippingRectangle {
                anchors.bottom: parent.bottom
                width: parent.width
                height: Math.min(parent.height, bar.total / Math.max(1, root.max) * parent.height)
                radius: Math.min(root.barRadius, width / 2)
                color: "transparent"
                opacity: root.hovered >= 0 && !bar.lit ? 0.6 : 1

                Behavior on height {
                    NumberAnimation { duration: 400; easing.type: Easing.OutCubic }
                }

                Behavior on opacity {
                    NumberAnimation { duration: 120 }
                }

                Column {
                    anchors.bottom: parent.bottom
                    width: parent.width

                    Repeater {
                        model: bar.stack.slice().reverse()

                        Rectangle {
                            required property var modelData

                            width: bar.width
                            height: bar.total > 0 ? modelData.seconds / bar.total * bar.height * Math.min(1, bar.total / Math.max(1, root.max)) : 0
                            color: modelData.color
                            opacity: root.filter && root.filter !== modelData.id ? 0.18 : 1

                            Behavior on opacity {
                                NumberAnimation { duration: 160 }
                            }
                        }
                    }
                }
            }

            MouseArea {
                anchors.fill: parent
                anchors.leftMargin: -root.spacing / 2
                anchors.rightMargin: -root.spacing / 2
                enabled: root.interactive
                hoverEnabled: true
                cursorShape: Qt.PointingHandCursor
                onContainsMouseChanged: if (containsMouse)
                    root.hovered = bar.index;
                else if (root.hovered === bar.index)
                    root.hovered = -1
                onClicked: root.clicked(bar.index)
            }
        }
    }
}
