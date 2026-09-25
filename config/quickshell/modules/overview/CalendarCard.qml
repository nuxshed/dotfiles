pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import "../../components"
import "../../config"
import "../../services"

Rectangle {
    id: root

    property date shown: new Date()

    readonly property var locale: Qt.locale("en_GB")

    function shift(months: int): void {
        root.shown = new Date(root.shown.getFullYear(), root.shown.getMonth() + months, 1);
    }

    radius: 12
    color: Colors.background

    WheelHandler {
        onWheel: event => root.shift(event.angleDelta.y > 0 ? -1 : 1)
    }

    ColumnLayout {
        anchors.fill: parent
        anchors.margins: 12
        anchors.topMargin: 10
        spacing: 4

        RowLayout {
            Layout.fillWidth: true
            spacing: 2

            Text {
                Layout.fillWidth: true
                text: root.locale.standaloneMonthName(root.shown.getMonth()) + " " + root.shown.getFullYear()
                color: Colors.textBright
                font.pixelSize: 13
                font.family: Fonts.family
                font.weight: Font.Medium

                TapHandler {
                    onTapped: root.shown = new Date()
                }
            }

            NavButton { icon: "chevron_left"; onActivated: root.shift(-1) }
            NavButton { icon: "chevron_right"; onActivated: root.shift(1) }
        }

        DayOfWeekRow {
            Layout.fillWidth: true
            locale: root.locale

            delegate: Text {
                required property var model

                text: model.shortName.slice(0, 2)
                color: Colors.textMuted
                font.pixelSize: 10
                font.family: Fonts.family
                horizontalAlignment: Text.AlignHCenter
            }
        }

        MonthGrid {
            id: grid

            Layout.fillWidth: true
            Layout.fillHeight: true
            month: root.shown.getMonth()
            year: root.shown.getFullYear()
            locale: root.locale

            delegate: Item {
                id: day

                required property var model

                readonly property bool weekend: model.date.getDay() === 0 || model.date.getDay() === 6
                readonly property var dayEvents: Calendar.eventsOn(model.date)
                readonly property bool selected: Qt.formatDate(model.date, "yyyy-MM-dd") === Qt.formatDate(Overview.selected, "yyyy-MM-dd")

                Rectangle {
                    anchors.centerIn: parent
                    width: 22
                    height: 22
                    radius: 7
                    color: day.model.today ? Colors.primary : day.selected ? Colors.surfaceActive : dayHover.hovered ? Colors.surface : "transparent"

                    Behavior on color {
                        ColorAnimation { duration: 120 }
                    }

                    Text {
                        anchors.centerIn: parent
                        text: day.model.day
                        color: day.model.today ? Colors.primaryText
                            : day.model.month !== grid.month ? Colors.subtle
                            : day.weekend ? Colors.textMuted : Colors.text
                        font.pixelSize: 11
                        font.family: Fonts.family
                        font.weight: day.model.today ? Font.Medium : Font.Normal
                    }
                }

                Row {
                    anchors.horizontalCenter: parent.horizontalCenter
                    anchors.bottom: parent.bottom
                    spacing: 2

                    Repeater {
                        model: day.dayEvents.slice(0, 3)

                        Rectangle {
                            required property var modelData

                            width: 3
                            height: 3
                            radius: 1.5
                            color: modelData.color
                        }
                    }
                }

                HoverHandler {
                    id: dayHover
                    cursorShape: Qt.PointingHandCursor
                }

                TapHandler {
                    onTapped: Overview.selected = day.model.date
                    onDoubleTapped: Calendar.show(day.model.date)
                }
            }
        }
    }

    component NavButton: Rectangle {
        id: btn

        property string icon: ""

        signal activated

        Layout.preferredWidth: 24
        Layout.preferredHeight: 24
        radius: 12
        color: area.containsMouse ? Colors.surfaceActive : "transparent"

        Behavior on color {
            ColorAnimation { duration: 140 }
        }

        MaterialIcon {
            anchors.centerIn: parent
            text: btn.icon
            size: 15
            color: area.containsMouse ? Colors.textBright : Colors.textMuted
        }

        MouseArea {
            id: area
            anchors.fill: parent
            hoverEnabled: true
            cursorShape: Qt.PointingHandCursor
            onClicked: btn.activated()
        }
    }
}
