pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import Quickshell
import "../../components"
import "../../config"
import "../../services"
import "../preview"

FloatingWindow {
    id: root

    property string view: "day"
    property date cursor: new Date()
    property real openKey: -1
    property real confirmKey: -1

    readonly property real dayStart: {
        const d = new Date(cursor);
        d.setHours(0, 0, 0, 0);
        return d.getTime();
    }
    readonly property real weekStart: {
        const d = new Date(dayStart);
        d.setDate(d.getDate() - (d.getDay() + 6) % 7);
        return d.getTime();
    }
    readonly property real rangeStart: view === "day" ? dayStart : weekStart
    readonly property real rangeEnd: view === "day" ? dayStart + 86400000 : new Date(weekStart).setDate(new Date(weekStart).getDate() + 7)
    readonly property var sessions: Pomodoro.sessionsBetween(rangeStart, rangeEnd)
    readonly property var past: Pomodoro.historyBetween(rangeStart, rangeEnd).slice().reverse()
    readonly property bool liveToday: Pomodoro.active && view === "day" && Pomodoro.session.start < rangeEnd && Pomodoro.now > rangeStart
    readonly property var stats: Pomodoro.totals(sessions, rangeStart, rangeEnd)
    readonly property var days: {
        const out = [];
        for (let i = 0; i < 7; i++) {
            const d = new Date(weekStart);
            d.setDate(d.getDate() + i);
            const a = d.getTime();
            const b = new Date(a).setDate(d.getDate() + 1);
            const list = Pomodoro.sessionsBetween(a, b);
            out.push({ date: d, start: a, end: b, sessions: list, totals: Pomodoro.totals(list, a, b) });
        }
        return out;
    }
    readonly property real maxFocus: Math.max(3600, ...days.map(d => d.totals.focus))
    readonly property string heading: {
        if (view === "day") {
            const key = Qt.formatDate(cursor, "yyyy-MM-dd");
            const today = new Date();
            if (key === Qt.formatDate(today, "yyyy-MM-dd"))
                return "Today";
            today.setDate(today.getDate() - 1);
            if (key === Qt.formatDate(today, "yyyy-MM-dd"))
                return "Yesterday";
            return Qt.formatDate(cursor, "dddd, d MMMM");
        }
        const a = new Date(weekStart);
        const b = new Date(weekStart);
        b.setDate(b.getDate() + 6);
        return a.getMonth() === b.getMonth() ? Qt.formatDate(a, "d") + " – " + Qt.formatDate(b, "d MMMM yyyy") : Qt.formatDate(a, "d MMM") + " – " + Qt.formatDate(b, "d MMM yyyy");
    }

    function step(dir: int): void {
        const d = new Date(root.cursor);
        d.setDate(d.getDate() + dir * (root.view === "day" ? 1 : 7));
        root.cursor = d;
    }

    function segmentsOf(list: var): var {
        return list.reduce((all, s) => all.concat(s.segments), []);
    }

    title: "Focus"
    visible: Pomodoro.appOpen
    implicitWidth: 1100
    implicitHeight: 760
    color: Colors.background

    onVisibleChanged: if (visible) {
        root.cursor = new Date();
        keys.forceActiveFocus();
    } else
        Pomodoro.appOpen = false

    Item {
        id: keys

        anchors.fill: parent
        focus: true

        Keys.onPressed: event => {
            if (event.key === Qt.Key_Escape)
                Pomodoro.appOpen = false;
            else if (event.key === Qt.Key_Tab || event.key === Qt.Key_Backtab)
                root.view = root.view === "day" ? "week" : "day";
            else if (event.key === Qt.Key_Left || event.key === Qt.Key_H)
                root.step(-1);
            else if (event.key === Qt.Key_Right || event.key === Qt.Key_L)
                root.step(1);
            else if (event.key === Qt.Key_T)
                root.cursor = new Date();
            else if (event.key === Qt.Key_Space)
                Pomodoro.active ? Pomodoro.toggle() : Pomodoro.start();
            else
                return;
            event.accepted = true;
        }

        ColumnLayout {
            anchors.fill: parent
            anchors.margins: 16
            spacing: 16

            RowLayout {
                Layout.fillWidth: true
                Layout.fillHeight: false
                Layout.preferredHeight: 34
                spacing: 6

                TextButton {
                    text: "Today"
                    onClicked: root.cursor = new Date()
                }

                PreviewButton {
                    implicitWidth: 30
                    implicitHeight: 30
                    icon: "chevron_left"
                    onClicked: root.step(-1)
                }

                PreviewButton {
                    implicitWidth: 30
                    implicitHeight: 30
                    icon: "chevron_right"
                    onClicked: root.step(1)
                }

                Text {
                    Layout.leftMargin: 6
                    Layout.fillWidth: true
                    text: root.heading
                    color: Colors.textBright
                    font.pixelSize: 18
                    font.family: Fonts.family
                    font.weight: Font.Medium
                    elide: Text.ElideRight
                }

                Segmented {
                    Layout.preferredWidth: 150
                    implicitHeight: 30
                    items: ["Day", "Week"]
                    currentIndex: root.view === "day" ? 0 : 1
                    onSelected: index => root.view = index === 0 ? "day" : "week"
                }

                TextButton {
                    text: Pomodoro.active ? Pomodoro.label + "  " + Pomodoro.display : "Start session"
                    primary: !Pomodoro.active
                    onClicked: if (!Pomodoro.active)
                        Pomodoro.start()
                    else
                        Overview.open = true
                }
            }

            RowLayout {
                Layout.fillWidth: true
                Layout.fillHeight: false
                spacing: 12

                Stat { label: "Focused"; value: Pomodoro.duration(root.stats.focus); tint: Colors.primary }
                Stat { label: "Breaks"; value: Pomodoro.duration(root.stats.break); tint: Colors.green }
                Stat { label: "Paused"; value: Pomodoro.duration(root.stats.pause); tint: Colors.outline }
                Stat { label: "Longest block"; value: Pomodoro.duration(root.stats.longest) }
                Stat { label: "Sessions"; value: String(root.stats.sessions) }
            }

            Rectangle {
                Layout.fillWidth: true
                Layout.fillHeight: false
                implicitHeight: tracks.implicitHeight + 32
                radius: 12
                color: Colors.surface

                ColumnLayout {
                    id: tracks

                    anchors.fill: parent
                    anchors.margins: 16
                    spacing: 10

                    Item {
                        id: hours

                        Layout.fillWidth: true
                        Layout.leftMargin: 60
                        implicitHeight: 14

                        Repeater {
                            model: 9

                            Text {
                                required property int index

                                x: index / 8 * hours.width - width / 2
                                text: String(index * 3).padStart(2, "0")
                                color: Colors.textMuted
                                font.pixelSize: 10
                                font.family: Fonts.family
                            }
                        }
                    }

                    Repeater {
                        model: root.view === "day" ? root.days.filter(d => d.start === root.dayStart) : root.days

                        RowLayout {
                            id: track

                            required property var modelData

                            readonly property bool today: Qt.formatDate(modelData.date, "yyyy-MM-dd") === Qt.formatDate(new Date(), "yyyy-MM-dd")

                            Layout.fillWidth: true
                            Layout.fillHeight: false
                            spacing: 0

                            Text {
                                Layout.preferredWidth: 60
                                text: root.view === "day" ? "Timeline" : Qt.formatDate(track.modelData.date, "ddd d")
                                color: track.today ? Colors.textBright : Colors.textMuted
                                font.pixelSize: 11
                                font.family: Fonts.family
                                font.weight: track.today ? Font.Medium : Font.Normal
                            }

                            Item {
                                id: lane

                                Layout.fillWidth: true
                                implicitHeight: root.view === "day" ? 22 : 12

                                Repeater {
                                    model: 7

                                    Rectangle {
                                        required property int index

                                        x: (index + 1) / 8 * lane.width
                                        width: 1
                                        height: lane.height
                                        color: Colors.border
                                    }
                                }

                                SegmentBar {
                                    anchors.fill: parent
                                    segments: root.segmentsOf(track.modelData.sessions)
                                    from: track.modelData.start
                                    to: track.modelData.end
                                    now: Pomodoro.now
                                    gap: 0
                                    opacity: 0.95
                                }

                                MouseArea {
                                    anchors.fill: parent
                                    enabled: root.view === "week"
                                    cursorShape: enabled ? Qt.PointingHandCursor : Qt.ArrowCursor
                                    onClicked: {
                                        root.cursor = track.modelData.date;
                                        root.view = "day";
                                    }
                                }
                            }
                        }
                    }

                    RowLayout {
                        Layout.leftMargin: 60
                        spacing: 14

                        Legend { label: "Focus"; kind: "focus" }
                        Legend { label: "Break"; kind: "break" }
                        Legend { label: "Pause"; kind: "pause" }
                    }
                }
            }

            Rectangle {
                visible: root.view === "week"
                Layout.fillWidth: true
                Layout.fillHeight: true
                radius: 12
                color: Colors.surface

                RowLayout {
                    anchors.fill: parent
                    anchors.margins: 16
                    spacing: 12

                    Repeater {
                        model: root.days

                        ColumnLayout {
                            id: col

                            required property var modelData

                            readonly property bool today: Qt.formatDate(modelData.date, "yyyy-MM-dd") === Qt.formatDate(new Date(), "yyyy-MM-dd")

                            Layout.fillWidth: true
                            Layout.fillHeight: true
                            spacing: 8

                            Text {
                                Layout.alignment: Qt.AlignHCenter
                                text: col.modelData.totals.focus > 0 ? Pomodoro.duration(col.modelData.totals.focus) : "–"
                                color: Colors.textDimmed
                                font.pixelSize: 11
                                font.family: Fonts.family
                            }

                            Item {
                                Layout.fillWidth: true
                                Layout.fillHeight: true

                                Rectangle {
                                    anchors.horizontalCenter: parent.horizontalCenter
                                    anchors.bottom: parent.bottom
                                    width: Math.min(44, parent.width * 0.6)
                                    height: Math.max(4, col.modelData.totals.focus / root.maxFocus * parent.height)
                                    radius: 8
                                    color: barArea.containsMouse ? Qt.tint(Colors.primary, "#22ffffff") : col.today ? Colors.primary : Qt.alpha(Colors.primary, 0.55)

                                    Behavior on height {
                                        NumberAnimation { duration: 300; easing.type: Easing.OutCubic }
                                    }
                                }

                                MouseArea {
                                    id: barArea
                                    anchors.fill: parent
                                    hoverEnabled: true
                                    cursorShape: Qt.PointingHandCursor
                                    onClicked: {
                                        root.cursor = col.modelData.date;
                                        root.view = "day";
                                    }
                                }
                            }

                            Text {
                                Layout.alignment: Qt.AlignHCenter
                                text: Qt.formatDate(col.modelData.date, "ddd")
                                color: col.today ? Colors.textBright : Colors.textMuted
                                font.pixelSize: 11
                                font.family: Fonts.family
                                font.weight: col.today ? Font.Medium : Font.Normal
                            }
                        }
                    }
                }
            }

            Rectangle {
                visible: root.view === "day"
                Layout.fillWidth: true
                Layout.fillHeight: true
                radius: 12
                color: Colors.surface

                ColumnLayout {
                    anchors.fill: parent
                    anchors.margins: 8
                    spacing: 4

                    RowLayout {
                        Layout.fillWidth: true
                        Layout.fillHeight: false
                        Layout.preferredHeight: 32
                        Layout.leftMargin: 10
                        Layout.rightMargin: 10
                        spacing: 8

                        Text {
                            text: "Sessions"
                            color: Colors.textBright
                            font.pixelSize: 13
                            font.family: Fonts.family
                            font.weight: Font.Medium
                        }

                        Text {
                            Layout.fillWidth: true
                            text: root.past.length + (root.liveToday ? 1 : 0)
                            color: Colors.textMuted
                            font.pixelSize: 12
                            font.family: Fonts.family
                        }

                        Text {
                            text: "Click a session to edit it"
                            color: Colors.textMuted
                            font.pixelSize: 11
                            font.family: Fonts.family
                        }
                    }

                    ListView {
                        id: list

                        Layout.fillWidth: true
                        Layout.fillHeight: true
                        clip: true
                        spacing: 4
                        boundsBehavior: Flickable.StopAtBounds
                        model: root.past

                        header: Item {
                            width: list.width
                            height: root.liveToday ? live.height + 4 : 0
                            visible: root.liveToday

                            Rectangle {
                                id: live

                                width: parent.width
                                height: 52
                                radius: 10
                                color: Colors.surfaceActive

                                RowLayout {
                                    anchors.fill: parent
                                    anchors.leftMargin: 14
                                    anchors.rightMargin: 14
                                    spacing: 14

                                    Item {
                                        Layout.preferredWidth: 16
                                        implicitHeight: 8

                                        Rectangle {
                                            anchors.centerIn: parent
                                            width: 8
                                            height: 8
                                            radius: 4
                                            color: Pomodoro.status === "break" ? Colors.green : Pomodoro.status === "paused" ? Colors.textMuted : Colors.primary

                                            SequentialAnimation on opacity {
                                                running: root.liveToday
                                                loops: Animation.Infinite
                                                NumberAnimation { to: 0.35; duration: 900; easing.type: Easing.InOutSine }
                                                NumberAnimation { to: 1; duration: 900; easing.type: Easing.InOutSine }
                                            }
                                        }
                                    }

                                    Column {
                                        Layout.preferredWidth: 110
                                        spacing: 2

                                        Text {
                                            text: Pomodoro.active ? Qt.formatTime(new Date(Pomodoro.session.start), "HH:mm") + " – now" : ""
                                            color: Colors.textBright
                                            font.pixelSize: 12
                                            font.family: Fonts.family
                                            font.weight: Font.Medium
                                        }

                                        Text {
                                            text: Pomodoro.label + " · " + Pomodoro.display
                                            color: Colors.textMuted
                                            font.pixelSize: 10
                                            font.family: Fonts.family
                                            font.features: { "tnum": 1 }
                                        }
                                    }

                                    SegmentBar {
                                        Layout.fillWidth: true
                                        segments: Pomodoro.session?.segments ?? []
                                        from: Pomodoro.session?.start ?? 0
                                        to: Pomodoro.now
                                        now: Pomodoro.now
                                    }

                                    Text {
                                        Layout.preferredWidth: 220
                                        horizontalAlignment: Text.AlignRight
                                        text: "In progress"
                                        color: Colors.textMuted
                                        font.pixelSize: 11
                                        font.family: Fonts.family
                                    }

                                    Item { implicitWidth: 16 }
                                }
                            }
                        }

                        delegate: Rectangle {
                            id: row

                            required property var modelData

                            readonly property real key: Pomodoro.keyOf(modelData)
                            readonly property bool open: root.openKey === key
                            readonly property var totals: Pomodoro.totals([modelData], modelData.start, modelData.end)

                            width: list.width
                            height: head.height + (open ? body.implicitHeight : 0)
                            radius: 10
                            color: open ? Colors.background : headHover.hovered ? Colors.surfaceActive : "transparent"
                            clip: true

                            Behavior on color {
                                ColorAnimation { duration: 120 }
                            }

                            Item {
                                id: head

                                width: parent.width
                                height: 52

                                HoverHandler {
                                    id: headHover
                                    cursorShape: Qt.PointingHandCursor
                                }

                                TapHandler {
                                    onTapped: {
                                        root.openKey = row.open ? -1 : row.key;
                                        root.confirmKey = -1;
                                    }
                                }

                                RowLayout {
                                    anchors.fill: parent
                                    anchors.leftMargin: 14
                                    anchors.rightMargin: 14
                                    spacing: 14

                                    MaterialIcon {
                                        Layout.preferredWidth: 16
                                        text: row.open ? "expand_more" : "chevron_right"
                                        size: 16
                                        color: headHover.hovered || row.open ? Colors.textBright : Colors.textMuted
                                    }

                                    Column {
                                        Layout.preferredWidth: 110
                                        spacing: 2

                                        Text {
                                            text: Qt.formatTime(new Date(row.modelData.start), "HH:mm") + " – " + Qt.formatTime(new Date(row.modelData.end), "HH:mm")
                                            color: Colors.textBright
                                            font.pixelSize: 12
                                            font.family: Fonts.family
                                            font.weight: Font.Medium
                                            font.features: { "tnum": 1 }
                                        }

                                        Text {
                                            text: Pomodoro.duration((row.modelData.end - row.modelData.start) / 1000)
                                            color: Colors.textMuted
                                            font.pixelSize: 10
                                            font.family: Fonts.family
                                        }
                                    }

                                    SegmentBar {
                                        Layout.fillWidth: true
                                        segments: row.modelData.segments
                                        from: row.modelData.start
                                        to: row.modelData.end
                                    }

                                    Text {
                                        Layout.preferredWidth: 220
                                        horizontalAlignment: Text.AlignRight
                                        text: Pomodoro.duration(row.totals.focus) + " focus" + (row.totals.pause > 0 ? "  ·  " + Pomodoro.duration(row.totals.pause) + " paused" : "") + (row.totals.break > 0 ? "  ·  " + Pomodoro.duration(row.totals.break) + " break" : "")
                                        color: Colors.textMuted
                                        font.pixelSize: 11
                                        font.family: Fonts.family
                                        elide: Text.ElideLeft
                                    }

                                    Item { implicitWidth: 16 }
                                }
                            }

                            ColumnLayout {
                                id: body

                                y: head.height
                                width: parent.width
                                spacing: 0
                                visible: row.open

                                Rectangle {
                                    Layout.fillWidth: true
                                    Layout.leftMargin: 14
                                    Layout.rightMargin: 14
                                    implicitHeight: 1
                                    color: Colors.border
                                }

                                Item { implicitHeight: 6 }

                                Repeater {
                                    model: row.open ? row.modelData.segments : []

                                    Rectangle {
                                        id: seg

                                        required property var modelData
                                        required property int index

                                        Layout.fillWidth: true
                                        Layout.leftMargin: 8
                                        Layout.rightMargin: 8
                                        implicitHeight: 36
                                        radius: 8
                                        color: segHover.hovered ? Colors.surface : "transparent"

                                        HoverHandler {
                                            id: segHover
                                        }

                                        RowLayout {
                                            anchors.fill: parent
                                            anchors.leftMargin: 6
                                            anchors.rightMargin: 6
                                            spacing: 6

                                            Rectangle {
                                                Layout.preferredWidth: 112
                                                implicitHeight: 26
                                                radius: 13
                                                color: kindArea.containsMouse ? Colors.surfaceActive : "transparent"

                                                Row {
                                                    anchors.verticalCenter: parent.verticalCenter
                                                    x: 10
                                                    spacing: 8

                                                    Rectangle {
                                                        anchors.verticalCenter: parent.verticalCenter
                                                        width: 8
                                                        height: 8
                                                        radius: 4
                                                        color: seg.modelData.kind === "focus" ? Colors.primary : seg.modelData.kind === "break" ? Colors.green : Colors.outline
                                                    }

                                                    Text {
                                                        anchors.verticalCenter: parent.verticalCenter
                                                        text: seg.modelData.kind === "focus" ? "Focus" : seg.modelData.kind === "pause" ? "Pause" : seg.modelData.long ? "Long break" : "Break"
                                                        color: Colors.text
                                                        font.pixelSize: 12
                                                        font.family: Fonts.family
                                                    }
                                                }

                                                MouseArea {
                                                    id: kindArea
                                                    anchors.fill: parent
                                                    hoverEnabled: true
                                                    cursorShape: Qt.PointingHandCursor
                                                    onClicked: Pomodoro.setSegmentKind(row.key, seg.index, { focus: "break", break: "pause", pause: "focus" }[seg.modelData.kind])
                                                }
                                            }

                                            TimeField {
                                                ms: seg.modelData.start
                                                onCommitted: value => Pomodoro.setSegmentTime(row.key, seg.index, "start", value)
                                                onFinished: keys.forceActiveFocus()
                                            }

                                            MaterialIcon {
                                                text: "arrow_forward"
                                                size: 12
                                                color: Colors.textMuted
                                            }

                                            TimeField {
                                                ms: seg.modelData.end
                                                onCommitted: value => Pomodoro.setSegmentTime(row.key, seg.index, "end", value)
                                                onFinished: keys.forceActiveFocus()
                                            }

                                            Item { Layout.fillWidth: true }

                                            Text {
                                                text: Pomodoro.clock((seg.modelData.end - seg.modelData.start) / 1000)
                                                color: Colors.textDimmed
                                                font.pixelSize: 12
                                                font.family: Fonts.family
                                                font.features: { "tnum": 1 }
                                            }

                                            Rectangle {
                                                implicitWidth: 26
                                                implicitHeight: 26
                                                radius: 13
                                                opacity: segHover.hovered ? 1 : 0
                                                color: segDel.containsMouse ? Qt.alpha(Colors.red, 0.18) : "transparent"

                                                Behavior on opacity {
                                                    NumberAnimation { duration: 120 }
                                                }

                                                MaterialIcon {
                                                    anchors.centerIn: parent
                                                    text: "close"
                                                    size: 14
                                                    color: segDel.containsMouse ? Colors.red : Colors.textMuted
                                                }

                                                MouseArea {
                                                    id: segDel
                                                    anchors.fill: parent
                                                    hoverEnabled: true
                                                    cursorShape: Qt.PointingHandCursor
                                                    onClicked: Pomodoro.removeSegment(row.key, seg.index)
                                                }
                                            }
                                        }
                                    }
                                }

                                RowLayout {
                                    Layout.fillWidth: true
                                    Layout.leftMargin: 14
                                    Layout.rightMargin: 14
                                    Layout.topMargin: 4
                                    Layout.bottomMargin: 10
                                    implicitHeight: 30
                                    spacing: 6

                                    Text {
                                        Layout.fillWidth: true
                                        text: root.confirmKey === row.key ? "Delete this session? This can't be undone." : "Click a type to change it, or a time to edit it"
                                        color: root.confirmKey === row.key ? Colors.red : Colors.textMuted
                                        font.pixelSize: 11
                                        font.family: Fonts.family
                                    }

                                    Ghost {
                                        visible: root.confirmKey === row.key
                                        text: "Cancel"
                                        onClicked: root.confirmKey = -1
                                    }

                                    Ghost {
                                        text: root.confirmKey === row.key ? "Delete" : "Delete session"
                                        danger: true
                                        solid: root.confirmKey === row.key
                                        onClicked: {
                                            if (root.confirmKey === row.key) {
                                                root.confirmKey = -1;
                                                root.openKey = -1;
                                                Pomodoro.removeSession(row.key);
                                            } else
                                                root.confirmKey = row.key;
                                        }
                                    }
                                }
                            }
                        }

                        ColumnLayout {
                            anchors.centerIn: parent
                            visible: list.count === 0 && !root.liveToday
                            spacing: 12

                            Text {
                                Layout.alignment: Qt.AlignHCenter
                                text: "No focus sessions"
                                color: Colors.textMuted
                                font.pixelSize: 13
                                font.family: Fonts.family
                            }

                            TextButton {
                                visible: !Pomodoro.active
                                Layout.alignment: Qt.AlignHCenter
                                text: "Start session"
                                primary: true
                                onClicked: Pomodoro.start()
                            }
                        }
                    }
                }
            }
        }
    }

    component Ghost: Rectangle {
        id: ghost

        property string text: ""
        property bool danger: false
        property bool solid: false

        signal clicked

        implicitWidth: ghostLabel.implicitWidth + 24
        implicitHeight: 28
        radius: 8
        color: ghost.solid ? Colors.red : ghostArea.containsMouse ? (ghost.danger ? Qt.alpha(Colors.red, 0.15) : Colors.surfaceActive) : "transparent"

        Behavior on color {
            ColorAnimation { duration: 120 }
        }

        Text {
            id: ghostLabel
            anchors.centerIn: parent
            text: ghost.text
            color: ghost.solid ? Colors.background : ghost.danger && ghostArea.containsMouse ? Colors.red : Colors.textDimmed
            font.pixelSize: 12
            font.family: Fonts.family
            font.weight: Font.Medium
        }

        MouseArea {
            id: ghostArea
            anchors.fill: parent
            hoverEnabled: true
            cursorShape: Qt.PointingHandCursor
            onClicked: ghost.clicked()
        }
    }

    component Stat: Rectangle {
        id: stat

        property string label: ""
        property string value: ""
        property color tint: "transparent"

        Layout.fillWidth: true
        implicitHeight: 72
        radius: 12
        color: Colors.surface

        ColumnLayout {
            anchors.fill: parent
            anchors.margins: 14
            spacing: 4

            RowLayout {
                spacing: 6

                Rectangle {
                    visible: stat.tint.a > 0
                    implicitWidth: 7
                    implicitHeight: 7
                    radius: 3.5
                    color: stat.tint
                }

                Text {
                    text: stat.label
                    color: Colors.textMuted
                    font.pixelSize: 11
                    font.family: Fonts.family
                }
            }

            Text {
                text: stat.value
                color: Colors.textBright
                font.pixelSize: 20
                font.family: Fonts.family
                font.weight: Font.Medium
            }
        }
    }

    component TimeField: Rectangle {
        id: tf

        property real ms: 0
        property bool editable: true

        signal committed(real value)
        signal finished

        implicitWidth: 52
        implicitHeight: 24
        radius: 6
        color: input.activeFocus ? Colors.background : tfHover.hovered && tf.editable ? Colors.surface : "transparent"
        border.width: input.activeFocus ? 1 : 0
        border.color: Colors.outline

        HoverHandler {
            id: tfHover
            cursorShape: tf.editable ? Qt.IBeamCursor : Qt.ArrowCursor
        }

        TextInput {
            id: input

            anchors.fill: parent
            horizontalAlignment: TextInput.AlignHCenter
            verticalAlignment: TextInput.AlignVCenter
            text: Qt.formatTime(new Date(tf.ms), "HH:mm")
            readOnly: !tf.editable
            color: Colors.text
            selectionColor: Colors.primaryContainer
            selectedTextColor: Colors.textBright
            font.pixelSize: 11
            font.family: Fonts.family
            font.features: { "tnum": 1 }
            selectByMouse: true
            maximumLength: 5
            validator: RegularExpressionValidator { regularExpression: /^\d{0,2}:?\d{0,2}$/ }

            function reset(): void {
                text = Qt.binding(() => Qt.formatTime(new Date(tf.ms), "HH:mm"));
            }

            onAccepted: {
                const m = text.match(/^(\d{1,2}):?(\d{2})$/);
                if (m && +m[1] < 24 && +m[2] < 60) {
                    const d = new Date(tf.ms);
                    d.setHours(+m[1], +m[2], 0, 0);
                    tf.committed(d.getTime());
                }
                reset();
                tf.finished();
            }
            onActiveFocusChanged: if (activeFocus)
                selectAll();
            else
                reset()

            Keys.onEscapePressed: event => {
                event.accepted = true;
                tf.finished();
            }
        }
    }

    component Legend: RowLayout {
        id: legend

        property string label: ""
        property string kind: ""

        spacing: 6

        Rectangle {
            implicitWidth: 8
            implicitHeight: 8
            radius: 4
            color: legend.kind === "focus" ? Colors.primary : legend.kind === "break" ? Colors.green : Colors.outline
        }

        Text {
            text: legend.label
            color: Colors.textMuted
            font.pixelSize: 10
            font.family: Fonts.family
        }
    }

    component TextButton: Rectangle {
        id: tb

        property string text: ""
        property bool primary: false

        signal clicked

        implicitWidth: tbLabel.implicitWidth + 24
        implicitHeight: 30
        radius: 8
        color: tb.primary ? Colors.primary : tbArea.containsMouse ? Colors.surfaceActive : Colors.surface
        opacity: tb.primary && tbArea.containsMouse ? 0.88 : 1

        Behavior on color {
            ColorAnimation { duration: 140 }
        }

        Text {
            id: tbLabel
            anchors.centerIn: parent
            text: tb.text
            color: tb.primary ? Colors.primaryText : Colors.text
            font.pixelSize: 12
            font.family: Fonts.family
            font.weight: tb.primary ? Font.Medium : Font.Normal
            font.features: { "tnum": 1 }
        }

        MouseArea {
            id: tbArea
            anchors.fill: parent
            hoverEnabled: true
            cursorShape: Qt.PointingHandCursor
            onClicked: tb.clicked()
        }
    }
}
