pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import "../../components"
import "../../config"
import "../../services"

Item {
    id: root

    readonly property date first: {
        const d = new Date(Calendar.cursor.getFullYear(), Calendar.cursor.getMonth(), 1);
        d.setDate(1 - (d.getDay() + 6) % 7);
        return d;
    }
    readonly property string todayKey: Qt.formatDate(new Date(), "yyyy-MM-dd")
    readonly property string selectedKey: Qt.formatDate(Calendar.selected, "yyyy-MM-dd")

    ColumnLayout {
        anchors.fill: parent
        spacing: 0

        Row {
            Layout.fillWidth: true
            height: 24

            Repeater {
                model: ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]

                Text {
                    required property string modelData

                    width: parent.width / 7
                    text: modelData
                    color: Colors.textMuted
                    font.pixelSize: 11
                    font.family: Fonts.family
                    horizontalAlignment: Text.AlignHCenter
                }
            }
        }

        Rectangle {
            Layout.fillWidth: true
            Layout.fillHeight: true
            radius: 12
            color: Colors.surface
            clip: true

            Grid {
                id: grid

                anchors.fill: parent
                anchors.margins: 1
                columns: 7

                Repeater {
                    model: 42

                    Item {
                        id: cell

                        required property int index

                        readonly property date date: {
                            const d = new Date(root.first);
                            d.setDate(root.first.getDate() + cell.index);
                            return d;
                        }
                        readonly property string key: Qt.formatDate(date, "yyyy-MM-dd")
                        readonly property bool inMonth: date.getMonth() === Calendar.cursor.getMonth()
                        readonly property bool today: key === root.todayKey
                        readonly property bool selected: key === root.selectedKey
                        readonly property var events: Calendar.eventsOn(date)
                        readonly property int fit: events.length * 20 <= height - 34 ? events.length : Math.max(0, Math.floor((height - 50) / 20))
                        readonly property int overflow: events.length - fit

                        width: grid.width / 7
                        height: grid.height / 6

                        HoverHandler {
                            id: cellHover
                        }

                        Rectangle {
                            anchors.fill: parent
                            anchors.margins: 3
                            radius: 8
                            color: cell.selected ? Colors.surfaceActive : Colors.subtle
                            opacity: cell.selected ? 1 : cellHover.hovered ? 0.35 : 0

                            Behavior on opacity {
                                NumberAnimation { duration: 120 }
                            }
                        }

                        Rectangle {
                            visible: cell.index % 7 !== 0
                            width: 1
                            height: parent.height
                            color: Colors.border
                        }

                        Rectangle {
                            visible: cell.index >= 7
                            width: parent.width
                            height: 1
                            color: Colors.border
                        }

                        MouseArea {
                            anchors.fill: parent
                            onClicked: Calendar.selected = cell.date
                            onDoubleClicked: {
                                Calendar.selected = cell.date;
                                Calendar.draft(cell.date.getTime(), true);
                            }
                        }

                        ColumnLayout {
                            anchors.fill: parent
                            anchors.margins: 6
                            spacing: 2
                            opacity: cell.inMonth ? 1 : 0.45

                            Rectangle {
                                implicitWidth: Math.max(22, dayLabel.implicitWidth + 12)
                                implicitHeight: 22
                                radius: 11
                                color: cell.today ? Colors.primary : "transparent"

                                Text {
                                    id: dayLabel
                                    anchors.centerIn: parent
                                    text: cell.date.getDate() === 1 ? Qt.formatDate(cell.date, "d MMM") : cell.date.getDate()
                                    color: cell.today ? Colors.primaryText : cell.selected ? Colors.textBright : Colors.text
                                    font.pixelSize: 11
                                    font.family: Fonts.family
                                    font.weight: cell.today ? Font.Medium : Font.Normal
                                }
                            }

                            Repeater {
                                model: cell.events.slice(0, cell.fit)

                                EventChip {
                                    required property var modelData

                                    Layout.fillWidth: true
                                    event: modelData
                                }
                            }

                            Text {
                                visible: cell.overflow > 0
                                text: "+" + cell.overflow + " more"
                                color: Colors.textMuted
                                font.pixelSize: 10
                                font.family: Fonts.family
                                leftPadding: 6

                                MouseArea {
                                    anchors.fill: parent
                                    cursorShape: Qt.PointingHandCursor
                                    onClicked: {
                                        Calendar.goto(cell.date);
                                        Calendar.view = "day";
                                    }
                                }
                            }

                            Item { Layout.fillHeight: true }
                        }
                    }
                }
            }
        }
    }

    component EventChip: Rectangle {
        id: chip

        property var event: null

        readonly property bool past: event.end < Date.now()

        height: 18
        radius: 5
        color: chip.event.allDay ? Qt.alpha(chip.event.color, chipArea.containsMouse ? 0.4 : 0.25) : chipArea.containsMouse ? Qt.alpha(chip.event.color, 0.18) : "transparent"
        opacity: chip.past ? 0.55 : 1

        Behavior on color {
            ColorAnimation { duration: 120 }
        }

        RowLayout {
            anchors.fill: parent
            anchors.leftMargin: 5
            anchors.rightMargin: 5
            spacing: 5

            Rectangle {
                visible: !chip.event.allDay
                width: 6
                height: 6
                radius: 3
                color: chip.event.color
            }

            Text {
                visible: !chip.event.allDay
                text: Qt.formatTime(new Date(chip.event.start), "HH:mm")
                color: Colors.textMuted
                font.pixelSize: 10
                font.family: Fonts.family
            }

            Text {
                Layout.fillWidth: true
                text: chip.event.summary
                color: Colors.text
                font.pixelSize: 10
                font.family: Fonts.family
                elide: Text.ElideRight
            }
        }

        MouseArea {
            id: chipArea
            anchors.fill: parent
            hoverEnabled: true
            cursorShape: Qt.PointingHandCursor
            onClicked: Calendar.edit(chip.event)
        }
    }
}
