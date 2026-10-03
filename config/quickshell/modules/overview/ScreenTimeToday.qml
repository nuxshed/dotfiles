pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import "../../components"
import "../../config"
import "../../services"
import "../screentime"

Rectangle {
    id: root

    property bool active: true

    readonly property var stats: ScreenTime.todayStats
    readonly property var apps: stats.apps.slice(0, 4)
    readonly property real usual: active ? ScreenTime.usual(ScreenTime.today, ScreenTime.now) : -1
    readonly property real delta: stats.total - usual

    radius: 12
    color: Colors.surface

    ColumnLayout {
        anchors.fill: parent
        anchors.margins: 12
        anchors.topMargin: 10
        spacing: 10

        RowLayout {
            Layout.fillWidth: true
            spacing: 8

            ColumnLayout {
                Layout.fillWidth: true
                spacing: 0

                Text {
                    Layout.fillWidth: true
                    text: "Today"
                    color: Colors.textMuted
                    font.pixelSize: 11
                    font.family: Fonts.family
                }

                RowLayout {
                    spacing: 8

                    Text {
                        text: ScreenTime.format(root.stats.total)
                        color: Colors.textBright
                        font.pixelSize: 22
                        font.family: Fonts.family
                        font.weight: Font.Medium
                    }

                    Text {
                        Layout.alignment: Qt.AlignBaseline
                        visible: root.usual >= 0 && Math.abs(root.delta) >= 60
                        text: `${ScreenTime.format(Math.abs(root.delta))} ${root.delta > 0 ? "more" : "less"} than usual`
                        color: Colors.textMuted
                        font.pixelSize: 10
                        font.family: Fonts.family
                    }
                }
            }

            Rectangle {
                Layout.alignment: Qt.AlignTop
                implicitWidth: 26
                implicitHeight: 26
                radius: 8
                color: openArea.containsMouse ? Colors.surfaceActive : "transparent"

                MaterialIcon {
                    anchors.centerIn: parent
                    text: "open_in_new"
                    size: 15
                    color: openArea.containsMouse ? Colors.textBright : Colors.textMuted
                }

                MouseArea {
                    id: openArea
                    anchors.fill: parent
                    hoverEnabled: true
                    cursorShape: Qt.PointingHandCursor
                    onClicked: ScreenTime.show(ScreenTime.today, "day")
                }
            }
        }

        ColumnLayout {
            Layout.fillWidth: true
            spacing: 4

            StackBars {
                Layout.fillWidth: true
                Layout.preferredHeight: 44
                bars: root.stats.hours.map(cats => ({ cats }))
                max: 3600
                spacing: 2
                barRadius: 2
                current: new Date(ScreenTime.now).getHours()
            }

            Item {
                id: axis

                Layout.fillWidth: true
                implicitHeight: 12

                Repeater {
                    model: [0, 6, 12, 18]

                    Text {
                        required property int modelData

                        x: modelData / 24 * axis.width
                        text: String(modelData).padStart(2, "0")
                        color: Colors.textMuted
                        font.pixelSize: 9
                        font.family: Fonts.family
                    }
                }
            }
        }

        Repeater {
            model: root.apps

            RowLayout {
                id: row

                required property var modelData

                readonly property var info: ScreenTime.info(modelData.id)
                readonly property var lead: ScreenTime.breakdown(root.stats, modelData.id, modelData.seconds).filter(p => !p.rest)[0] ?? null

                Layout.fillWidth: true
                spacing: 8

                AppGlyph {
                    info: row.info
                    size: 16
                }

                Text {
                    Layout.maximumWidth: 130
                    text: row.info.name
                    color: Colors.text
                    font.pixelSize: 11
                    font.family: Fonts.family
                    elide: Text.ElideRight
                }

                Text {
                    Layout.fillWidth: true
                    text: row.lead?.label ?? ""
                    color: Colors.textMuted
                    font.pixelSize: 10
                    font.family: Fonts.family
                    elide: Text.ElideRight
                }

                Rectangle {
                    implicitWidth: 6
                    implicitHeight: 6
                    radius: 3
                    color: ScreenTime.categoryOf(row.info.category).color
                }

                Text {
                    Layout.preferredWidth: 44
                    horizontalAlignment: Text.AlignRight
                    text: ScreenTime.format(row.modelData.seconds)
                    color: Colors.textMuted
                    font.pixelSize: 10
                    font.family: Fonts.family
                    font.features: { "tnum": 1 }
                }
            }
        }

        Text {
            visible: root.apps.length === 0
            text: "Nothing tracked yet today"
            color: Colors.textMuted
            font.pixelSize: 11
            font.family: Fonts.family
        }

        Item { Layout.fillHeight: true }
    }
}
