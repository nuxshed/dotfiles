pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import "../../components"
import "../../config"
import "../../services"

Rectangle {
    id: root

    readonly property int total: ScreenTime.week.reduce((s, d) => s + d.seconds, 0)
    readonly property int max: Math.max(1, ...ScreenTime.week.map(d => d.seconds))
    readonly property int active: ScreenTime.week.filter(d => d.seconds > 0).length

    radius: 12
    color: Colors.surface

    ColumnLayout {
        anchors.fill: parent
        anchors.margins: 12
        anchors.topMargin: 10
        spacing: 10

        RowLayout {
            Layout.fillWidth: true

            ColumnLayout {
                Layout.fillWidth: true
                spacing: 0

                Text {
                    text: "This week"
                    color: Colors.textMuted
                    font.pixelSize: 11
                    font.family: Fonts.family
                }

                Text {
                    text: ScreenTime.format(root.total)
                    color: Colors.textBright
                    font.pixelSize: 22
                    font.family: Fonts.family
                    font.weight: Font.Medium
                }
            }

            Text {
                Layout.alignment: Qt.AlignBottom
                text: root.active > 0 ? ScreenTime.format(Math.round(root.total / root.active)) + " avg" : ""
                color: Colors.textMuted
                font.pixelSize: 10
                font.family: Fonts.family
            }
        }

        RowLayout {
            Layout.fillWidth: true
            Layout.fillHeight: true
            spacing: 6

            Repeater {
                model: ScreenTime.week

                ColumnLayout {
                    id: col

                    required property var modelData

                    Layout.fillWidth: true
                    Layout.fillHeight: true
                    spacing: 6

                    Item {
                        Layout.fillWidth: true
                        Layout.fillHeight: true

                        Rectangle {
                            anchors.bottom: parent.bottom
                            width: parent.width
                            height: Math.max(4, parent.height * col.modelData.seconds / root.max)
                            radius: 4
                            color: col.modelData.today ? Colors.primary : Colors.subtle

                            Behavior on height {
                                NumberAnimation { duration: 400; easing.type: Easing.OutCubic }
                            }
                        }
                    }

                    Text {
                        Layout.alignment: Qt.AlignHCenter
                        text: col.modelData.label
                        color: col.modelData.today ? Colors.textBright : Colors.textMuted
                        font.pixelSize: 10
                        font.family: Fonts.family
                    }
                }
            }
        }
    }
}
