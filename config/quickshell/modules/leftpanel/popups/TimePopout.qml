pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import "../../../services"
import "../../../config"
import "../../../components"

Popout {
    id: root

    property int offset: 0

    readonly property date day: {
        const d = new Date(Clock.key + "T00:00:00");
        d.setDate(d.getDate() + root.offset);
        return d;
    }
    readonly property real dayStart: day.getTime()
    readonly property var events: Calendar.eventsOn(day)
    readonly property var timed: events.filter(e => !e.allDay && e.end - e.start < 86400000)
    readonly property var allDay: events.filter(e => e.allDay || e.end - e.start >= 86400000)
    readonly property int fromHour: 0
    readonly property int toHour: 24
    readonly property int visibleHours: 5
    readonly property real hourHeight: flick.height / visibleHours
    readonly property real now: Clock.now.getTime()
    readonly property var laidOut: {
        const list = timed.slice().sort((a, b) => a.start - b.start || b.end - a.end);
        const lanes = [];
        const out = [];
        for (const e of list) {
            let lane = lanes.findIndex(endAt => endAt <= e.start);
            if (lane < 0) {
                lane = lanes.length;
                lanes.push(0);
            }
            lanes[lane] = e.end;
            out.push({ event: e, lane });
        }
        const count = Math.max(1, lanes.length);
        return out.map(o => Object.assign(o, { count }));
    }

    function yOf(ms: real): real {
        return Math.max(0, Math.min(chart.height, (ms - root.dayStart - root.fromHour * 3600000) / 3600000 * root.hourHeight));
    }

    function focusHour(): void {
        let h = 8;
        if (root.offset === 0)
            h = new Date().getHours() - 0.5;
        else if (root.timed.length > 0)
            h = new Date(root.timed[0].start).getHours() - 0.5;
        flick.contentY = Math.max(0, Math.min(flick.contentHeight - flick.height, h * root.hourHeight));
    }

    function label(): string {
        if (root.offset === 0)
            return "Today";
        if (root.offset === 1)
            return "Tomorrow";
        if (root.offset === -1)
            return "Yesterday";
        return root.offset > 0 ? "In " + root.offset + " days" : -root.offset + " days ago";
    }

    notch: 18
    contentWidth: 320
    contentHeight: 424

    onOpened: {
        root.offset = 0;
        Qt.callLater(root.focusHour);
    }
    onOffsetChanged: Qt.callLater(root.focusHour)

    ColumnLayout {
        id: column

        anchors.fill: parent
        anchors.margins: 18
        spacing: 12

        RowLayout {
            Layout.fillWidth: true
            Layout.preferredHeight: 38
            Layout.fillHeight: false
            spacing: 2

            WheelHandler {
                onWheel: event => root.offset += event.angleDelta.y > 0 ? -1 : 1
            }

            ColumnLayout {
                Layout.fillWidth: true
                Layout.fillHeight: false
                spacing: 2

                Text {
                    Layout.fillWidth: true
                    text: root.label()
                    color: titleHover.hovered ? Colors.primary : Colors.textBright
                    font.pixelSize: 15
                    font.family: Fonts.family
                    font.weight: Font.Medium
                    elide: Text.ElideRight

                    Behavior on color {
                        ColorAnimation { duration: 140 }
                    }

                    HoverHandler {
                        id: titleHover
                        cursorShape: Qt.PointingHandCursor
                    }

                    TapHandler {
                        onTapped: {
                            root.hide();
                            Calendar.show(root.day);
                        }
                    }
                }

                Text {
                    Layout.fillWidth: true
                    text: Qt.formatDate(root.day, root.day.getFullYear() === new Date().getFullYear() ? "dddd, d MMMM" : "dddd, d MMMM yyyy") + "  ·  " + (root.events.length === 0 ? "no events" : root.events.length === 1 ? "1 event" : root.events.length + " events")
                    color: Colors.textMuted
                    font.pixelSize: 11
                    font.family: Fonts.family
                    elide: Text.ElideRight
                }
            }

            Nav {
                icon: "today"
                size: 14
                opacity: root.offset !== 0 ? 1 : 0
                enabled: root.offset !== 0
                onActivated: root.offset = 0

                Behavior on opacity {
                    NumberAnimation { duration: 140 }
                }
            }

            Nav { icon: "chevron_left"; onActivated: root.offset-- }
            Nav { icon: "chevron_right"; onActivated: root.offset++ }
        }

        Item {
            Layout.fillWidth: true
            Layout.preferredHeight: 22
            Layout.fillHeight: false
            clip: true

            Row {
                spacing: 4

                Repeater {
                    model: root.allDay

                    Rectangle {
                        id: pill

                        required property var modelData

                        width: Math.min(column.width, pillLabel.implicitWidth + 16)
                        height: 22
                        radius: 6
                        color: Qt.alpha(pill.modelData.color, 0.22)

                        Text {
                            id: pillLabel
                            anchors.centerIn: parent
                            width: Math.min(implicitWidth, pill.width - 16)
                            text: pill.modelData.summary
                            color: Colors.textBright
                            font.pixelSize: 10
                            font.family: Fonts.family
                            elide: Text.ElideRight
                        }
                    }
                }
            }

            Text {
                visible: root.allDay.length === 0
                anchors.verticalCenter: parent.verticalCenter
                text: "No all-day events"
                color: Colors.subtle
                font.pixelSize: 10
                font.family: Fonts.family
            }
        }

        Item {
            Layout.fillWidth: true
            Layout.fillHeight: true

            Flickable {
                id: flick

                anchors.fill: parent
                contentWidth: width
                contentHeight: chart.height + 12
                boundsBehavior: Flickable.StopAtBounds
                clip: true

                Item {
                    id: chart

                    readonly property int gutter: 28

                    y: 6
                    width: flick.width
                    height: (root.toHour - root.fromHour) * root.hourHeight

                    Repeater {
                        model: root.toHour - root.fromHour + 1

                        Item {
                            id: tick

                            required property int index

                            readonly property int hour: root.fromHour + index

                            y: index * root.hourHeight
                            width: chart.width

                            Rectangle {
                                x: chart.gutter
                                width: parent.width - chart.gutter
                                height: 1
                                color: Colors.border
                            }

                            Text {
                                visible: tick.index < root.toHour - root.fromHour
                                y: -5
                                text: String(tick.hour).padStart(2, "0")
                                color: Colors.textMuted
                                font.pixelSize: 9
                                font.family: Fonts.family
                            }
                        }
                    }

                    Repeater {
                        model: root.laidOut

                        Rectangle {
                            id: block

                            required property var modelData

                            readonly property real laneWidth: (chart.width - chart.gutter - 2) / modelData.count
                            readonly property bool live: modelData.event.start <= root.now && modelData.event.end > root.now

                            x: chart.gutter + 2 + modelData.lane * laneWidth
                            y: root.yOf(modelData.event.start) + 1
                            width: laneWidth - 3
                            height: Math.max(16, root.yOf(modelData.event.end) - root.yOf(modelData.event.start) - 2)
                            radius: 6
                            color: Qt.tint(Colors.background, Qt.alpha(block.modelData.event.color, block.live ? 0.4 : 0.2))
                            opacity: block.modelData.event.end < root.now ? 0.55 : 1
                            clip: true

                            Rectangle {
                                width: 3
                                height: parent.height
                                color: block.modelData.event.color
                            }

                            Column {
                                anchors.fill: parent
                                anchors.leftMargin: 7
                                anchors.rightMargin: 4
                                anchors.topMargin: 2
                                spacing: 0

                                Text {
                                    width: parent.width
                                    text: block.modelData.event.summary
                                    color: Colors.textBright
                                    font.pixelSize: 10
                                    font.family: Fonts.family
                                    font.weight: Font.Medium
                                    elide: Text.ElideRight
                                }

                                Text {
                                    visible: block.height > 28
                                    width: parent.width
                                    text: Qt.formatTime(new Date(block.modelData.event.start), "HH:mm") + " – " + Qt.formatTime(new Date(block.modelData.event.end), "HH:mm")
                                    color: Colors.textDimmed
                                    font.pixelSize: 9
                                    font.family: Fonts.family
                                    elide: Text.ElideRight
                                }
                            }
                        }
                    }

                    Rectangle {
                        visible: root.offset === 0 && root.now > root.dayStart + root.fromHour * 3600000 && root.now < root.dayStart + root.toHour * 3600000
                        x: chart.gutter
                        y: root.yOf(root.now)
                        width: chart.width - chart.gutter
                        height: 2
                        color: Colors.red

                        Rectangle {
                            x: -3
                            y: -2
                            width: 6
                            height: 6
                            radius: 3
                            color: Colors.red
                        }
                    }
                }
            }

            Text {
                anchors.centerIn: parent
                visible: root.timed.length === 0
                text: "Nothing scheduled"
                color: Colors.textMuted
                font.pixelSize: 11
                font.family: Fonts.family
            }
        }
    }

    component Nav: Rectangle {
        id: nav

        property string icon: ""
        property int size: 15

        signal activated

        Layout.preferredWidth: 24
        Layout.preferredHeight: 24
        radius: 12
        color: navArea.containsMouse ? Colors.surfaceActive : "transparent"

        MaterialIcon {
            anchors.centerIn: parent
            text: nav.icon
            size: nav.size
            color: navArea.containsMouse ? Colors.textBright : Colors.textMuted
        }

        MouseArea {
            id: navArea
            anchors.fill: parent
            hoverEnabled: true
            cursorShape: Qt.PointingHandCursor
            onClicked: nav.activated()
        }
    }
}
