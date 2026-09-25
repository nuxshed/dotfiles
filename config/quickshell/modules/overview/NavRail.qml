pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import "../../components"
import "../../config"
import "../../services"

Item {
    id: root

    property bool shown: false

    implicitWidth: 34

    ColumnLayout {
        anchors.centerIn: parent
        spacing: 4

        Repeater {
            model: Overview.tabs

            Reveal {
                id: item

                required property var modelData
                required property int index

                readonly property bool active: Overview.tab === item.modelData.id

                Layout.preferredWidth: 32
                Layout.preferredHeight: 32
                shown: root.shown
                delay: 20 + index * 30

                Rectangle {
                    anchors.fill: parent
                    radius: 10
                    color: item.active ? Colors.primaryContainer : hover.hovered ? Colors.surface : "transparent"

                    Behavior on color {
                        ColorAnimation { duration: 140 }
                    }

                    MaterialIcon {
                        anchors.centerIn: parent
                        text: item.modelData.icon
                        size: 16
                        color: item.active ? Colors.primaryContainerText : Colors.textMuted
                    }

                    HoverHandler {
                        id: hover
                        cursorShape: Qt.PointingHandCursor
                    }

                    TapHandler {
                        onTapped: Overview.tab = item.modelData.id
                    }
                }
            }
        }
    }
}
