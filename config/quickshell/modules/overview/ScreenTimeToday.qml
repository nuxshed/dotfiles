pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import "../../components"
import "../../config"
import "../../services"

Rectangle {
    id: root

    readonly property var apps: ScreenTime.todayApps.slice(0, 5)
    readonly property int max: apps[0]?.seconds ?? 1

    radius: 12
    color: Colors.background

    ColumnLayout {
        anchors.fill: parent
        anchors.margins: 12
        anchors.topMargin: 10
        spacing: 10

        ColumnLayout {
            spacing: 0

            Text {
                text: "Today"
                color: Colors.textMuted
                font.pixelSize: 11
                font.family: Fonts.family
            }

            Text {
                text: ScreenTime.format(ScreenTime.todayTotal)
                color: Colors.textBright
                font.pixelSize: 22
                font.family: Fonts.family
                font.weight: Font.Medium
            }
        }

        Repeater {
            model: root.apps

            ColumnLayout {
                id: row

                required property var modelData

                Layout.fillWidth: true
                spacing: 3

                RowLayout {
                    Layout.fillWidth: true

                    Text {
                        Layout.fillWidth: true
                        text: row.modelData.name
                        color: Colors.text
                        font.pixelSize: 11
                        font.family: Fonts.family
                        elide: Text.ElideRight
                    }

                    Text {
                        text: ScreenTime.format(row.modelData.seconds)
                        color: Colors.textMuted
                        font.pixelSize: 10
                        font.family: Fonts.family
                    }
                }

                Rectangle {
                    Layout.fillWidth: true
                    height: 4
                    radius: 2
                    color: Colors.surfaceActive

                    Rectangle {
                        width: parent.width * row.modelData.seconds / root.max
                        height: parent.height
                        radius: 2
                        color: Colors.primary

                        Behavior on width {
                            NumberAnimation { duration: 400; easing.type: Easing.OutCubic }
                        }
                    }
                }
            }
        }

        Text {
            visible: root.apps.length === 0
            text: "nothing tracked yet"
            color: Colors.textMuted
            font.pixelSize: 11
            font.family: Fonts.family
        }

        Item { Layout.fillHeight: true }
    }
}
