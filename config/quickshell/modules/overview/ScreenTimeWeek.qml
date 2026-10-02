pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import "../../config"
import "../../services"
import "../screentime"

Rectangle {
    id: root

    readonly property string start: {
        const d = ScreenTime.dateOf(ScreenTime.today);
        d.setDate(d.getDate() - (d.getDay() + 6) % 7);
        return ScreenTime.key(d);
    }
    readonly property var days: ScreenTime.range(start, 7)
    readonly property var week: ScreenTime.merge(days)
    readonly property real avg: week.active > 0 ? week.total / week.active : 0
    readonly property real max: Math.max(3600, ...days.map(d => d.total))
    readonly property var cats: ScreenTime.categoryList.filter(c => (week.cats[c.id] ?? 0) > 0).sort((a, b) => week.cats[b.id] - week.cats[a.id]).slice(0, 3)

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
                    Layout.fillWidth: true
                    text: "This week"
                    color: Colors.textMuted
                    font.pixelSize: 11
                    font.family: Fonts.family
                }

                Text {
                    text: ScreenTime.format(root.week.total)
                    color: Colors.textBright
                    font.pixelSize: 22
                    font.family: Fonts.family
                    font.weight: Font.Medium
                }
            }

            Text {
                Layout.alignment: Qt.AlignBottom
                Layout.bottomMargin: 4
                visible: root.avg > 0
                text: ScreenTime.format(root.avg) + " daily avg"
                color: Colors.textMuted
                font.pixelSize: 10
                font.family: Fonts.family
            }
        }

        Item {
            Layout.fillWidth: true
            Layout.fillHeight: true

            StackBars {
                id: bars

                anchors.fill: parent
                bars: root.days
                max: root.max
                spacing: 8
                barRadius: 4
                interactive: true
                current: root.days.findIndex(d => d.key === ScreenTime.today)
                onClicked: index => ScreenTime.show(root.days[index].key, "day")
            }

            Row {
                visible: root.avg > 0
                y: parent.height - root.avg / root.max * parent.height
                width: parent.width
                spacing: 3

                Repeater {
                    model: Math.floor(parent.width / 6)

                    Rectangle {
                        width: 3
                        height: 1
                        color: Colors.textMuted
                        opacity: 0.6
                    }
                }
            }
        }

        RowLayout {
            Layout.fillWidth: true
            spacing: 8

            Repeater {
                model: root.days

                Text {
                    required property var modelData

                    Layout.fillWidth: true
                    Layout.preferredWidth: 1
                    horizontalAlignment: Text.AlignHCenter
                    text: Qt.formatDate(ScreenTime.dateOf(modelData.key), "ddd").slice(0, 2)
                    color: modelData.key === ScreenTime.today ? Colors.textBright : Colors.textMuted
                    font.pixelSize: 10
                    font.family: Fonts.family
                    font.weight: modelData.key === ScreenTime.today ? Font.Medium : Font.Normal
                }
            }
        }

        RowLayout {
            Layout.fillWidth: true
            spacing: 12

            Repeater {
                model: root.cats

                RowLayout {
                    id: chip

                    required property var modelData

                    spacing: 5

                    Rectangle {
                        implicitWidth: 6
                        implicitHeight: 6
                        radius: 3
                        color: chip.modelData.color
                    }

                    Text {
                        text: chip.modelData.name + " " + ScreenTime.format(root.week.cats[chip.modelData.id])
                        color: Colors.textMuted
                        font.pixelSize: 10
                        font.family: Fonts.family
                    }
                }
            }
        }
    }
}
