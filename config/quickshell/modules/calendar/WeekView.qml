pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import "../../components"
import "../../config"
import "../../services"

Item {
    id: root

    property int days: 7

    readonly property int hourHeight: 52
    readonly property int gutter: 46
    readonly property date first: {
        const d = new Date(Calendar.cursor);
        d.setHours(0, 0, 0, 0);
        if (root.days === 7)
            d.setDate(d.getDate() - (d.getDay() + 6) % 7);
        return d;
    }
    readonly property real end: first.getTime() + days * 86400000
    readonly property var visibleEvents: Calendar.eventsBetween(first.getTime(), end)
    readonly property var allDay: visibleEvents.filter(e => e.allDay || e.end - e.start >= 86400000)
    readonly property var timed: visibleEvents.filter(e => !(e.allDay || e.end - e.start >= 86400000))
    property real now: Date.now()
    readonly property real colWidth: (width - gutter) / days

    function laidOut(day: int): var {
        const dayStart = root.first.getTime() + day * 86400000;
        const dayEnd = dayStart + 86400000;
        const list = root.timed.filter(e => e.start < dayEnd && e.end > dayStart).sort((a, b) => a.start - b.start || b.end - a.end);
        const lanes = [];
        const out = [];
        for (const e of list) {
            let lane = lanes.findIndex(endAt => endAt <= e.start);
            if (lane < 0) {
                lane = lanes.length;
                lanes.push(0);
            }
            lanes[lane] = e.end;
            out.push({ event: e, lane, start: Math.max(e.start, dayStart), end: Math.min(e.end, dayEnd) });
        }
        const count = Math.max(1, lanes.length);
        return out.map(o => Object.assign(o, { count }));
    }

    Timer {
        interval: 60000
        running: root.visible
        repeat: true
        onTriggered: root.now = Date.now()
    }

    Component.onCompleted: flick.contentY = root.hourHeight * 7.5

    ColumnLayout {
        anchors.fill: parent
        spacing: 0

        Row {
            Layout.fillWidth: true
            Layout.leftMargin: root.gutter
            height: 40

            Repeater {
                model: root.days

                Item {
                    id: head

                    required property int index

                    readonly property date date: new Date(root.first.getTime() + index * 86400000)
                    readonly property bool today: Qt.formatDate(date, "yyyy-MM-dd") === Clock.key

                    width: root.colWidth
                    height: 40

                    Row {
                        anchors.centerIn: parent
                        spacing: 6

                        Text {
                            anchors.verticalCenter: parent.verticalCenter
                            text: Qt.formatDate(head.date, "ddd")
                            color: head.today ? Colors.textBright : Colors.textMuted
                            font.pixelSize: 11
                            font.family: Fonts.family
                        }

                        Rectangle {
                            anchors.verticalCenter: parent.verticalCenter
                            width: 24
                            height: 24
                            radius: 12
                            color: head.today ? Colors.primary : "transparent"

                            Text {
                                anchors.centerIn: parent
                                text: head.date.getDate()
                                color: head.today ? Colors.primaryText : Colors.text
                                font.pixelSize: 12
                                font.family: Fonts.family
                                font.weight: Font.Medium
                            }
                        }
                    }

                    TapHandler {
                        onTapped: {
                            Calendar.goto(head.date);
                            Calendar.view = "day";
                        }
                    }
                }
            }
        }

        Rectangle {
            visible: root.allDay.length > 0
            Layout.fillWidth: true
            implicitHeight: allDayCol.implicitHeight + 8
            color: "transparent"

            Column {
                id: allDayCol

                x: root.gutter
                y: 4
                width: parent.width - root.gutter
                spacing: 2

                Repeater {
                    model: root.allDay

                    Rectangle {
                        id: band

                        required property var modelData

                        readonly property int from: Math.max(0, Math.floor((modelData.start - root.first.getTime()) / 86400000))
                        readonly property int to: Math.min(root.days, Math.ceil((modelData.end - root.first.getTime()) / 86400000))

                        x: from * root.colWidth + 2
                        width: Math.max(root.colWidth, (to - from) * root.colWidth) - 4
                        height: 18
                        radius: 5
                        color: Qt.alpha(band.modelData.color, bandArea.containsMouse ? 0.4 : 0.25)

                        Text {
                            anchors.fill: parent
                            anchors.leftMargin: 6
                            anchors.rightMargin: 6
                            verticalAlignment: Text.AlignVCenter
                            text: band.modelData.summary
                            color: Colors.textBright
                            font.pixelSize: 10
                            font.family: Fonts.family
                            elide: Text.ElideRight
                        }

                        MouseArea {
                            id: bandArea
                            anchors.fill: parent
                            hoverEnabled: true
                            cursorShape: Qt.PointingHandCursor
                            onClicked: Calendar.edit(band.modelData)
                        }
                    }
                }
            }
        }

        Rectangle {
            Layout.fillWidth: true
            Layout.fillHeight: true
            radius: 12
            color: Colors.surface
            clip: true

            Flickable {
                id: flick

                anchors.fill: parent
                contentHeight: root.hourHeight * 24
                boundsBehavior: Flickable.StopAtBounds
                clip: true

                Item {
                    width: flick.width
                    height: root.hourHeight * 24

                    Repeater {
                        model: 24

                        Item {
                            id: hour

                            required property int index

                            y: index * root.hourHeight
                            width: parent.width
                            height: root.hourHeight

                            Rectangle {
                                x: root.gutter
                                width: parent.width - root.gutter
                                height: 1
                                color: Colors.border
                            }

                            Text {
                                visible: hour.index > 0
                                x: 8
                                y: -6
                                text: String(hour.index).padStart(2, "0") + ":00"
                                color: Colors.textMuted
                                font.pixelSize: 10
                                font.family: Fonts.family
                            }
                        }
                    }

                    Repeater {
                        model: root.days

                        Item {
                            id: col

                            required property int index

                            readonly property real dayStart: root.first.getTime() + index * 86400000

                            x: root.gutter + index * root.colWidth
                            width: root.colWidth
                            height: parent.height

                            Rectangle {
                                visible: col.index > 0
                                width: 1
                                height: parent.height
                                color: Colors.border
                            }

                            MouseArea {
                                anchors.fill: parent
                                onClicked: mouse => {
                                    const minutes = Math.floor(mouse.y / root.hourHeight * 2) * 30;
                                    Calendar.selected = new Date(col.dayStart);
                                    Calendar.draft(col.dayStart + minutes * 60000, false);
                                }
                            }

                            Repeater {
                                model: root.laidOut(col.index)

                                Rectangle {
                                    id: block

                                    required property var modelData

                                    readonly property real laneWidth: (col.width - 6) / modelData.count

                                    x: 3 + modelData.lane * laneWidth
                                    y: (modelData.start - col.dayStart) / 3600000 * root.hourHeight + 1
                                    width: laneWidth - 2
                                    height: Math.max(18, (modelData.end - modelData.start) / 3600000 * root.hourHeight - 2)
                                    radius: 6
                                    color: Qt.alpha(modelData.event.color, blockArea.containsMouse ? 0.4 : 0.25)
                                    opacity: modelData.event.end < root.now ? 0.6 : 1
                                    clip: true

                                    Rectangle {
                                        width: 3
                                        height: parent.height
                                        color: block.modelData.event.color
                                    }

                                    Column {
                                        anchors.fill: parent
                                        anchors.leftMargin: 8
                                        anchors.rightMargin: 4
                                        anchors.topMargin: 3
                                        spacing: 0

                                        Text {
                                            width: parent.width
                                            text: block.modelData.event.summary
                                            color: Colors.textBright
                                            font.pixelSize: 11
                                            font.family: Fonts.family
                                            font.weight: Font.Medium
                                            elide: Text.ElideRight
                                        }

                                        Text {
                                            visible: block.height > 32
                                            width: parent.width
                                            text: Qt.formatTime(new Date(block.modelData.event.start), "HH:mm") + " – " + Qt.formatTime(new Date(block.modelData.event.end), "HH:mm")
                                            color: Colors.textDimmed
                                            font.pixelSize: 10
                                            font.family: Fonts.family
                                            elide: Text.ElideRight
                                        }
                                    }

                                    MouseArea {
                                        id: blockArea
                                        anchors.fill: parent
                                        hoverEnabled: true
                                        cursorShape: Qt.PointingHandCursor
                                        onClicked: Calendar.edit(block.modelData.event)
                                    }
                                }
                            }
                        }
                    }

                    Rectangle {
                        readonly property int dayIndex: Math.floor((root.now - root.first.getTime()) / 86400000)

                        visible: dayIndex >= 0 && dayIndex < root.days
                        x: root.gutter + dayIndex * root.colWidth
                        y: (root.now - root.first.getTime() - dayIndex * 86400000) / 3600000 * root.hourHeight
                        width: root.colWidth
                        height: 2
                        color: Colors.red

                        Rectangle {
                            x: -4
                            y: -3
                            width: 8
                            height: 8
                            radius: 4
                            color: Colors.red
                        }
                    }
                }
            }
        }
    }
}
