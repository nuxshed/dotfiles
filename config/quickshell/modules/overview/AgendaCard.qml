pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import "../../components"
import "../../config"
import "../../services"

Rectangle {
    id: root

    readonly property string selectedKey: Qt.formatDate(Overview.selected, "yyyy-MM-dd")
    readonly property var model: {
        const from = new Date(Overview.selected);
        from.setHours(0, 0, 0, 0);
        const key = root.selectedKey;
        return Calendar.events.filter(e => e.end > from.getTime()).map(e => e.day < key ? Object.assign({}, e, { day: key }) : e).sort((a, b) => a.day.localeCompare(b.day) || (a.allDay === b.allDay ? a.start - b.start : a.allDay ? -1 : 1)).slice(0, 60);
    }

    function dayLabel(key: string): string {
        const today = Qt.formatDate(new Date(), "yyyy-MM-dd");
        const t = new Date();
        t.setDate(t.getDate() + 1);
        if (key === today)
            return "Today";
        if (key === Qt.formatDate(t, "yyyy-MM-dd"))
            return "Tomorrow";
        return Qt.formatDate(new Date(key + "T00:00:00"), "ddd d MMM");
    }

    function timeLabel(e: var): string {
        if (e.allDay)
            return "all day";
        const s = Qt.formatTime(new Date(e.start), "HH:mm");
        return e.end > e.start ? s + " – " + Qt.formatTime(new Date(e.end), "HH:mm") : s;
    }

    radius: 12
    color: Colors.background

    ColumnLayout {
        anchors.fill: parent
        anchors.margins: 12
        anchors.topMargin: 10
        spacing: 6

        RowLayout {
            Layout.fillWidth: true
            spacing: 2

            Text {
                Layout.fillWidth: true
                text: root.dayLabel(root.selectedKey)
                color: Colors.textBright
                font.pixelSize: 13
                font.family: Fonts.family
                font.weight: Font.Medium
            }

            Small {
                icon: "event"
                onActivated: Calendar.show(Overview.selected)
            }
        }

        ListView {
            id: list

            Layout.fillWidth: true
            Layout.fillHeight: true
            clip: true
            spacing: 2
            boundsBehavior: Flickable.StopAtBounds
            model: root.model

            section.property: "day"
            section.delegate: Item {
                id: header

                required property string section

                readonly property bool hide: section === root.selectedKey

                width: list.width
                height: hide ? 0 : 24
                opacity: hide ? 0 : 1

                Text {
                    anchors.bottom: parent.bottom
                    anchors.bottomMargin: 3
                    text: root.dayLabel(header.section)
                    color: Colors.textMuted
                    font.pixelSize: 10
                    font.family: Fonts.family
                    font.weight: Font.Medium
                }
            }

            delegate: Rectangle {
                id: row

                required property var modelData

                width: list.width
                height: 24
                radius: 6
                color: rowHover.hovered ? Colors.surface : "transparent"

                RowLayout {
                    anchors.fill: parent
                    anchors.leftMargin: 4
                    anchors.rightMargin: 6
                    spacing: 8

                    Rectangle {
                        width: 3
                        height: 14
                        radius: 1.5
                        color: row.modelData.color
                        opacity: row.modelData.allDay ? 0.5 : 1
                    }

                    Text {
                        Layout.fillWidth: true
                        text: row.modelData.summary
                        color: Colors.text
                        font.pixelSize: 11
                        font.family: Fonts.family
                        elide: Text.ElideRight
                    }

                    Text {
                        text: root.timeLabel(row.modelData)
                        color: Colors.textMuted
                        font.pixelSize: 10
                        font.family: Fonts.family
                    }
                }

                HoverHandler {
                    id: rowHover
                    cursorShape: Qt.PointingHandCursor
                }

                TapHandler {
                    onTapped: {
                        Calendar.show(new Date(row.modelData.start));
                        Calendar.edit(row.modelData);
                    }
                }
            }

            Text {
                anchors.centerIn: parent
                visible: list.count === 0
                text: Agenda.loading ? "loading…" : "nothing coming up"
                color: Colors.textMuted
                font.pixelSize: 11
                font.family: Fonts.family
            }
        }
    }

    component Small: Rectangle {
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
            size: 14
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
