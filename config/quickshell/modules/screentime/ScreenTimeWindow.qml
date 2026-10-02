pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Hyprland
import Quickshell.Widgets
import "../../components"
import "../../config"
import "../../services"
import "../preview"

FloatingWindow {
    id: root

    property string filter: ""
    property string openApp: ""

    readonly property string view: ScreenTime.view
    readonly property string today: ScreenTime.today
    readonly property string cursor: ScreenTime.cursor || today
    readonly property string start: {
        const d = ScreenTime.dateOf(cursor);
        if (view === "week")
            d.setDate(d.getDate() - (d.getDay() + 6) % 7);
        else if (view === "month")
            d.setDate(1);
        return ScreenTime.key(d);
    }
    readonly property int count: {
        if (view === "day")
            return 1;
        if (view === "week")
            return 7;
        const d = ScreenTime.dateOf(start);
        return new Date(d.getFullYear(), d.getMonth() + 1, 0).getDate();
    }
    readonly property var list: ScreenTime.range(start, count)
    readonly property var agg: ScreenTime.merge(list)
    readonly property var prev: ScreenTime.merge(ScreenTime.range(ScreenTime.shift(start, view === "month" ? -ScreenTime.dateOf(ScreenTime.shift(start, -1)).getDate() : -count), view === "month" ? ScreenTime.dateOf(ScreenTime.shift(start, -1)).getDate() : count))
    readonly property real avg: agg.active > 0 ? agg.total / agg.active : 0
    readonly property real prevAvg: prev.active > 0 ? prev.total / prev.active : 0
    readonly property bool isToday: view === "day" && cursor === today
    readonly property real usual: view !== "day" ? -1 : isToday ? ScreenTime.usual(today, ScreenTime.now) : ScreenTime.average(cursor, 7)
    readonly property real dayStart: ScreenTime.dateOf(cursor).getTime()
    readonly property var focus: view === "day" ? Pomodoro.sessionsBetween(dayStart, dayStart + 86400000).reduce((all, s) => all.concat(s.segments), []).filter(s => s.kind === "focus") : []
    readonly property var bars: view === "day" ? agg.hours.map((cats, h) => ({ cats, hour: h })) : list.map(d => ({ cats: d.cats, key: d.key, total: d.total }))
    readonly property real max: view === "day" ? 3600 : Math.max(3600, Math.ceil(Math.max(...list.map(d => d.total)) / 3600) * 3600)
    readonly property var apps: filter ? agg.apps.map(a => ({ id: a.id, seconds: agg.appCats[a.id]?.[filter] ?? 0 })).filter(a => a.seconds > 0).sort((a, b) => b.seconds - a.seconds) : agg.apps
    readonly property var cats: ScreenTime.categoryList.filter(c => (agg.cats[c.id] ?? 0) > 0).sort((a, b) => agg.cats[b.id] - agg.cats[a.id])
    readonly property string heading: {
        if (view === "day") {
            if (cursor === today)
                return "Today";
            if (cursor === ScreenTime.shift(today, -1))
                return "Yesterday";
            return Qt.formatDate(ScreenTime.dateOf(cursor), "dddd, d MMMM");
        }
        if (view === "month")
            return Qt.formatDate(ScreenTime.dateOf(start), "MMMM yyyy");
        const a = ScreenTime.dateOf(start);
        const b = ScreenTime.dateOf(ScreenTime.shift(start, 6));
        return a.getMonth() === b.getMonth() ? Qt.formatDate(a, "d") + " – " + Qt.formatDate(b, "d MMMM yyyy") : Qt.formatDate(a, "d MMM") + " – " + Qt.formatDate(b, "d MMM yyyy");
    }

    function step(dir: int): void {
        const d = ScreenTime.dateOf(root.cursor);
        if (root.view === "month")
            d.setMonth(d.getMonth() + dir, 1);
        else
            d.setDate(d.getDate() + dir * (root.view === "day" ? 1 : 7));
        ScreenTime.cursor = ScreenTime.key(d);
    }

    function setView(v: string): void {
        ScreenTime.view = v;
        root.openApp = "";
    }

    function hourLabel(h: int): string {
        return String(h).padStart(2, "0") + ":00";
    }

    function compare(value: real, base: real, noun: string): string {
        if (base <= 0 || value <= 0 || root.agg.active < 2)
            return "";
        const pct = Math.round((value / base - 1) * 100);
        return pct === 0 ? "Same as " + noun : `${Math.abs(pct)}% ${pct > 0 ? "more" : "less"} than ${noun}`;
    }

    title: "Screen Time"
    visible: ScreenTime.appOpen
    implicitWidth: 1100
    implicitHeight: 760
    color: Colors.background

    onVisibleChanged: if (visible) {
        keys.forceActiveFocus();
        maximize.restart();
    } else {
        ScreenTime.appOpen = false;
        ScreenTime.cursor = "";
        root.filter = "";
        root.openApp = "";
    }

    Timer {
        id: maximize
        interval: 120
        onTriggered: Hyprland.dispatch("hl.dsp.window.fullscreen({ window = 'title:^Screen Time$', mode = 'maximized', action = 'set' })")
    }

    Item {
        id: keys

        anchors.fill: parent
        focus: true

        Keys.onPressed: event => {
            const views = ["day", "week", "month"];
            if (event.key === Qt.Key_Escape)
                ScreenTime.appOpen = false;
            else if (event.key === Qt.Key_Tab)
                root.setView(views[(views.indexOf(root.view) + 1) % 3]);
            else if (event.key === Qt.Key_Backtab)
                root.setView(views[(views.indexOf(root.view) + 2) % 3]);
            else if (event.key === Qt.Key_Left || event.key === Qt.Key_H)
                root.step(-1);
            else if (event.key === Qt.Key_Right || event.key === Qt.Key_L)
                root.step(1);
            else if (event.key === Qt.Key_T)
                ScreenTime.cursor = "";
            else if (event.key === Qt.Key_D)
                root.setView("day");
            else if (event.key === Qt.Key_W)
                root.setView("week");
            else if (event.key === Qt.Key_M)
                root.setView("month");
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
                Layout.preferredHeight: 34
                spacing: 6

                TextButton {
                    text: "Today"
                    onClicked: ScreenTime.cursor = ""
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
                    Layout.preferredWidth: 220
                    implicitHeight: 30
                    items: ["Day", "Week", "Month"]
                    currentIndex: ["day", "week", "month"].indexOf(root.view)
                    onSelected: index => root.setView(["day", "week", "month"][index])
                }
            }

            RowLayout {
                Layout.fillWidth: true
                spacing: 12

                Stat {
                    label: "Screen time"
                    value: ScreenTime.format(root.agg.total)
                    note: root.view === "day" ? (root.usual >= 0 && Math.abs(root.agg.total - root.usual) >= 60 ? `${ScreenTime.format(Math.abs(root.agg.total - root.usual))} ${root.agg.total > root.usual ? "more" : "less"} than ${root.isToday ? "usual by now" : "average"}` : "") : root.compare(root.avg, root.prevAvg, "last " + root.view)
                }

                Stat {
                    visible: root.view !== "day"
                    label: "Daily average"
                    value: ScreenTime.format(root.avg)
                    note: root.agg.active + (root.agg.active === 1 ? " active day" : " active days")
                }

                Stat {
                    visible: root.view === "day"
                    label: "First use"
                    value: ScreenTime.clock(root.list[0]?.first ?? 0)
                    note: root.list[0]?.last > 0 && !root.isToday ? "Last at " + ScreenTime.clock(root.list[0].last) : ""
                }

                Stat {
                    label: "Pickups"
                    value: String(root.agg.pickups)
                    note: root.view !== "day" && root.agg.active > 0 ? Math.round(root.agg.pickups / root.agg.active) + " a day" : ""
                }

                Stat {
                    label: "Longest stretch"
                    value: ScreenTime.format(root.agg.longest)
                    note: "Without a break"
                }

                Stat {
                    readonly property var best: root.agg.apps[0] ? ScreenTime.info(root.agg.apps[0].id) : null

                    label: "Most used"
                    value: best?.name ?? "–"
                    note: best ? ScreenTime.format(root.agg.apps[0].seconds) : ""
                }
            }

            Rectangle {
                Layout.fillWidth: true
                Layout.preferredHeight: 290
                radius: 12
                color: Colors.surface

                ColumnLayout {
                    anchors.fill: parent
                    anchors.margins: 16
                    spacing: 10

                    RowLayout {
                        Layout.fillWidth: true
                        spacing: 14

                        Text {
                            Layout.fillWidth: true
                            text: root.view === "day" ? "By hour" : "By day"
                            color: Colors.textBright
                            font.pixelSize: 13
                            font.family: Fonts.family
                            font.weight: Font.Medium
                        }

                        Repeater {
                            model: ScreenTime.categoryList.filter(c => (root.agg.cats[c.id] ?? 0) > 0)

                            Chip {
                                required property var modelData

                                label: modelData.name
                                dot: modelData.color
                                active: root.filter === modelData.id
                                dimmed: root.filter !== "" && !active
                                onClicked: root.filter = active ? "" : modelData.id
                            }
                        }

                        Chip {
                            visible: root.focus.length > 0
                            label: "Focus sessions"
                            dot: Colors.primary
                            enabled: false
                        }
                    }

                    RowLayout {
                        Layout.fillWidth: true
                        Layout.fillHeight: true
                        spacing: 10

                        Item {
                            id: plot

                            Layout.fillWidth: true
                            Layout.fillHeight: true

                            Repeater {
                                model: [1, 0.5]

                                Rectangle {
                                    required property real modelData

                                    y: plot.height - modelData * plot.height
                                    width: plot.width
                                    height: 1
                                    color: Colors.border
                                }
                            }

                            StackBars {
                                id: chart

                                anchors.fill: parent
                                bars: root.bars
                                max: root.max
                                spacing: root.view === "day" ? 4 : root.view === "week" ? 18 : 5
                                barRadius: root.view === "week" ? 6 : 3
                                filter: root.filter
                                interactive: true
                                current: root.view === "day" ? (root.isToday ? new Date(ScreenTime.now).getHours() : -1) : root.list.findIndex(d => d.key === root.today)
                                onClicked: index => {
                                    if (root.view === "day")
                                        return;
                                    ScreenTime.cursor = root.bars[index].key;
                                    root.setView("day");
                                }
                            }

                            Row {
                                visible: root.view !== "day" && root.avg > 0
                                y: plot.height - root.avg / root.max * plot.height
                                spacing: 4

                                Repeater {
                                    model: Math.floor(plot.width / 8)

                                    Rectangle {
                                        width: 4
                                        height: 1
                                        color: Colors.textMuted
                                    }
                                }
                            }

                            Rectangle {
                                id: tip

                                readonly property var bar: chart.hovered >= 0 ? root.bars[chart.hovered] : null
                                readonly property real total: bar ? Object.values(bar.cats).reduce((s, v) => s + v, 0) : 0
                                readonly property var lines: bar ? ScreenTime.categoryList.filter(c => (bar.cats[c.id] ?? 0) > 0).sort((a, b) => bar.cats[b.id] - bar.cats[a.id]).slice(0, 4) : []

                                visible: bar !== null
                                x: Math.max(0, Math.min(plot.width - width, chart.barX(chart.hovered) + chart.barWidth / 2 - width / 2))
                                y: 0
                                z: 10
                                width: tipCol.implicitWidth + 20
                                height: tipCol.implicitHeight + 16
                                radius: 8
                                color: Colors.surfaceActive
                                border.width: 1
                                border.color: Colors.border

                                ColumnLayout {
                                    id: tipCol

                                    anchors.centerIn: parent
                                    spacing: 4

                                    Text {
                                        text: !tip.bar ? "" : root.view === "day" ? root.hourLabel(tip.bar.hour) + " – " + root.hourLabel((tip.bar.hour + 1) % 24) : Qt.formatDate(ScreenTime.dateOf(tip.bar.key), "ddd d MMM")
                                        color: Colors.textMuted
                                        font.pixelSize: 10
                                        font.family: Fonts.family
                                    }

                                    Text {
                                        text: ScreenTime.format(tip.total)
                                        color: Colors.textBright
                                        font.pixelSize: 14
                                        font.family: Fonts.family
                                        font.weight: Font.Medium
                                    }

                                    Repeater {
                                        model: tip.lines

                                        RowLayout {
                                            id: line

                                            required property var modelData

                                            spacing: 6

                                            Rectangle {
                                                implicitWidth: 6
                                                implicitHeight: 6
                                                radius: 3
                                                color: line.modelData.color
                                            }

                                            Text {
                                                Layout.preferredWidth: 90
                                                text: line.modelData.name
                                                color: Colors.text
                                                font.pixelSize: 11
                                                font.family: Fonts.family
                                            }

                                            Text {
                                                text: ScreenTime.format(tip.bar.cats[line.modelData.id])
                                                color: Colors.textMuted
                                                font.pixelSize: 11
                                                font.family: Fonts.family
                                                font.features: { "tnum": 1 }
                                            }
                                        }
                                    }
                                }
                            }
                        }

                        Item {
                            Layout.preferredWidth: 44
                            Layout.fillHeight: true

                            Repeater {
                                model: [1, 0.5]

                                Text {
                                    required property real modelData

                                    y: parent.height - modelData * parent.height - height / 2
                                    text: root.view === "day" ? (modelData === 1 ? "60m" : "30m") : ScreenTime.format(root.max * modelData)
                                    color: Colors.textMuted
                                    font.pixelSize: 10
                                    font.family: Fonts.family
                                }
                            }
                        }
                    }

                    Item {
                        id: axis

                        Layout.fillWidth: true
                        Layout.rightMargin: 54
                        implicitHeight: 12

                        Repeater {
                            model: root.bars

                            Text {
                                required property int index
                                required property var modelData

                                readonly property bool shown: root.view !== "day" || index % 3 === 0
                                readonly property bool here: root.view === "day" ? root.isToday && index === new Date(ScreenTime.now).getHours() : modelData.key === root.today

                                visible: shown
                                x: chart.barX(index) + chart.barWidth / 2 - width / 2
                                text: root.view === "day" ? String(index).padStart(2, "0") : root.view === "week" ? Qt.formatDate(ScreenTime.dateOf(modelData.key), "ddd d") : String(ScreenTime.dateOf(modelData.key).getDate())
                                color: here ? Colors.textBright : Colors.textMuted
                                font.pixelSize: 10
                                font.family: Fonts.family
                                font.weight: here ? Font.Medium : Font.Normal
                            }
                        }
                    }

                    SegmentBar {
                        visible: root.focus.length > 0
                        Layout.fillWidth: true
                        Layout.rightMargin: 54
                        implicitHeight: 4
                        segments: root.focus
                        from: root.dayStart
                        to: root.dayStart + 86400000
                        now: Pomodoro.now
                        gap: 0
                    }
                }
            }

            RowLayout {
                Layout.fillWidth: true
                Layout.fillHeight: true
                spacing: 16

                Rectangle {
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
                            Layout.preferredHeight: 32
                            Layout.leftMargin: 10
                            Layout.rightMargin: 10
                            spacing: 8

                            Text {
                                text: "Apps"
                                color: Colors.textBright
                                font.pixelSize: 13
                                font.family: Fonts.family
                                font.weight: Font.Medium
                            }

                            Text {
                                Layout.fillWidth: true
                                text: root.apps.length
                                color: Colors.textMuted
                                font.pixelSize: 12
                                font.family: Fonts.family
                            }

                            Chip {
                                visible: root.filter !== ""
                                label: ScreenTime.categoryOf(root.filter).name
                                dot: ScreenTime.categoryOf(root.filter).color
                                closable: true
                                active: true
                                onClicked: root.filter = ""
                            }
                        }

                        ListView {
                            id: appList

                            Layout.fillWidth: true
                            Layout.fillHeight: true
                            clip: true
                            spacing: 2
                            boundsBehavior: Flickable.StopAtBounds
                            model: root.apps

                            delegate: AppRow {}
                        }

                        Text {
                            visible: root.apps.length === 0
                            Layout.alignment: Qt.AlignHCenter
                            Layout.bottomMargin: 40
                            text: "No screen time recorded"
                            color: Colors.textMuted
                            font.pixelSize: 12
                            font.family: Fonts.family
                        }
                    }
                }

                Rectangle {
                    Layout.preferredWidth: 320
                    Layout.fillHeight: true
                    radius: 12
                    color: Colors.surface

                    ColumnLayout {
                        anchors.fill: parent
                        anchors.margins: 8
                        spacing: 2

                        Text {
                            Layout.preferredHeight: 32
                            Layout.leftMargin: 10
                            verticalAlignment: Text.AlignVCenter
                            text: "Categories"
                            color: Colors.textBright
                            font.pixelSize: 13
                            font.family: Fonts.family
                            font.weight: Font.Medium
                        }

                        Repeater {
                            model: root.cats

                            Rectangle {
                                id: cat

                                required property var modelData

                                readonly property real seconds: root.agg.cats[modelData.id] ?? 0
                                readonly property bool active: root.filter === modelData.id

                                Layout.fillWidth: true
                                implicitHeight: 44
                                radius: 10
                                color: active ? Colors.surfaceActive : catHover.hovered ? Colors.surfaceActive : "transparent"
                                opacity: root.filter !== "" && !active ? 0.5 : 1

                                HoverHandler {
                                    id: catHover
                                    cursorShape: Qt.PointingHandCursor
                                }

                                TapHandler {
                                    onTapped: root.filter = cat.active ? "" : cat.modelData.id
                                }

                                ColumnLayout {
                                    anchors.fill: parent
                                    anchors.leftMargin: 12
                                    anchors.rightMargin: 12
                                    anchors.topMargin: 8
                                    anchors.bottomMargin: 8
                                    spacing: 5

                                    RowLayout {
                                        Layout.fillWidth: true
                                        spacing: 8

                                        Rectangle {
                                            implicitWidth: 7
                                            implicitHeight: 7
                                            radius: 3.5
                                            color: cat.modelData.color
                                        }

                                        Text {
                                            Layout.fillWidth: true
                                            text: cat.modelData.name
                                            color: Colors.text
                                            font.pixelSize: 12
                                            font.family: Fonts.family
                                        }

                                        Text {
                                            text: ScreenTime.format(cat.seconds)
                                            color: Colors.textBright
                                            font.pixelSize: 12
                                            font.family: Fonts.family
                                            font.features: { "tnum": 1 }
                                        }

                                        Text {
                                            Layout.preferredWidth: 34
                                            horizontalAlignment: Text.AlignRight
                                            text: Math.round(cat.seconds / Math.max(1, root.agg.total) * 100) + "%"
                                            color: Colors.textMuted
                                            font.pixelSize: 11
                                            font.family: Fonts.family
                                            font.features: { "tnum": 1 }
                                        }
                                    }

                                    Rectangle {
                                        Layout.fillWidth: true
                                        implicitHeight: 3
                                        radius: 1.5
                                        color: Colors.surfaceActive

                                        Rectangle {
                                            width: parent.width * cat.seconds / Math.max(1, root.agg.total)
                                            height: parent.height
                                            radius: 1.5
                                            color: cat.modelData.color

                                            Behavior on width {
                                                NumberAnimation { duration: 400; easing.type: Easing.OutCubic }
                                            }
                                        }
                                    }
                                }
                            }
                        }

                        Item { Layout.fillHeight: true }

                        Text {
                            Layout.fillWidth: true
                            Layout.margins: 10
                            text: "Counts time the focused window is in use. Idle after " + ScreenTime.idleTimeout / 60 + " min and while locked is excluded."
                            color: Colors.textMuted
                            font.pixelSize: 10
                            font.family: Fonts.family
                            wrapMode: Text.WordWrap
                        }
                    }
                }
            }
        }
    }

    component AppRow: Rectangle {
        id: row

        required property var modelData

        readonly property var info: ScreenTime.info(modelData.id)
        readonly property var category: ScreenTime.categoryOf(info.category)
        readonly property bool open: root.openApp === modelData.id
        readonly property real lead: root.apps[0]?.seconds ?? 1
        readonly property var trend: !open ? [] : root.view === "day" ? (root.agg.appHours[modelData.id] ?? []) : root.list.map(d => d.apps.find(a => a.id === row.modelData.id)?.seconds ?? 0)
        readonly property int peak: trend.indexOf(Math.max(...trend))
        readonly property real full: root.agg.apps.find(a => a.id === modelData.id)?.seconds ?? modelData.seconds
        readonly property var parts: ScreenTime.breakdown(root.agg, modelData.id, full).filter(p => !root.filter || (p.rest ? row.info.category : ScreenTime.categoryFor(modelData.id, p.label)) === root.filter)

        property string editing: ""

        width: ListView.view.width
        height: head.height + (open ? body.implicitHeight : 0)
        radius: 10
        color: open ? Colors.background : rowHover.hovered ? Colors.surfaceActive : "transparent"
        clip: true

        Behavior on color {
            ColorAnimation { duration: 120 }
        }

        Item {
            id: head

            width: parent.width
            height: 48

            HoverHandler {
                id: rowHover
                cursorShape: Qt.PointingHandCursor
            }

            TapHandler {
                onTapped: root.openApp = row.open ? "" : row.modelData.id
            }

            RowLayout {
                anchors.fill: parent
                anchors.leftMargin: 12
                anchors.rightMargin: 14
                spacing: 12

                AppGlyph {
                    info: row.info
                    size: 24
                }

                ColumnLayout {
                    Layout.fillWidth: false
                    Layout.preferredWidth: 200
                    spacing: 1

                    Text {
                        Layout.fillWidth: true
                        text: row.info.name
                        color: Colors.textBright
                        font.pixelSize: 12
                        font.family: Fonts.family
                        font.weight: Font.Medium
                        elide: Text.ElideRight
                    }

                    RowLayout {
                        spacing: 5

                        Rectangle {
                            implicitWidth: 6
                            implicitHeight: 6
                            radius: 3
                            color: row.category.color
                        }

                        Text {
                            Layout.maximumWidth: 180
                            text: row.category.name + (row.parts.length > 0 && !row.parts[0].rest ? "  ·  " + row.parts.filter(p => !p.rest).slice(0, 2).map(p => p.label).join(", ") : "")
                            color: Colors.textMuted
                            font.pixelSize: 10
                            font.family: Fonts.family
                            elide: Text.ElideRight
                        }
                    }
                }

                Rectangle {
                    Layout.fillWidth: true
                    implicitHeight: 4
                    radius: 2
                    color: Colors.surfaceActive

                    ClippingRectangle {
                        width: parent.width * row.modelData.seconds / row.lead
                        height: parent.height
                        radius: 2
                        color: "transparent"

                        Behavior on width {
                            NumberAnimation { duration: 400; easing.type: Easing.OutCubic }
                        }

                        Row {
                            id: segs

                            anchors.fill: parent

                            Repeater {
                                model: root.filter ? [{ color: ScreenTime.categoryOf(root.filter).color, share: 1 }] : ScreenTime.categoryList.filter(c => (root.agg.appCats[row.modelData.id]?.[c.id] ?? 0) > 0).map(c => ({ color: c.color, share: root.agg.appCats[row.modelData.id][c.id] / Math.max(1, row.modelData.seconds) }))

                                Rectangle {
                                    required property var modelData

                                    width: segs.width * modelData.share
                                    height: segs.height
                                    color: modelData.color
                                }
                            }
                        }
                    }
                }

                Text {
                    Layout.preferredWidth: 64
                    horizontalAlignment: Text.AlignRight
                    text: ScreenTime.format(row.modelData.seconds)
                    color: Colors.textBright
                    font.pixelSize: 12
                    font.family: Fonts.family
                    font.features: { "tnum": 1 }
                }

                Text {
                    Layout.preferredWidth: 36
                    horizontalAlignment: Text.AlignRight
                    text: Math.round(row.modelData.seconds / Math.max(1, root.agg.total) * 100) + "%"
                    color: Colors.textMuted
                    font.pixelSize: 11
                    font.family: Fonts.family
                    font.features: { "tnum": 1 }
                }

                MaterialIcon {
                    text: row.open ? "expand_more" : "chevron_right"
                    size: 16
                    color: rowHover.hovered || row.open ? Colors.textBright : Colors.textMuted
                }
            }
        }

        ColumnLayout {
            id: body

            y: head.height
            width: parent.width
            visible: row.open
            spacing: 12

            Rectangle {
                Layout.fillWidth: true
                Layout.leftMargin: 14
                Layout.rightMargin: 14
                implicitHeight: 1
                color: Colors.border
            }

            RowLayout {
                Layout.fillWidth: true
                Layout.leftMargin: 14
                Layout.rightMargin: 14
                spacing: 16

                StackBars {
                    Layout.fillWidth: true
                    Layout.preferredHeight: 56
                    bars: row.trend.map(s => ({ cats: { [row.info.category]: s } }))
                    max: Math.max(60, ...row.trend)
                    spacing: root.view === "week" ? 10 : 3
                    barRadius: 2
                }

                ColumnLayout {
                    Layout.preferredWidth: 180
                    spacing: 3

                    Text {
                        text: root.view === "day" ? "Most active" : "Daily average"
                        color: Colors.textMuted
                        font.pixelSize: 10
                        font.family: Fonts.family
                    }

                    Text {
                        text: root.view === "day" ? (row.peak >= 0 ? root.hourLabel(row.peak) + " – " + root.hourLabel((row.peak + 1) % 24) : "–") : ScreenTime.format(row.modelData.seconds / Math.max(1, row.trend.filter(s => s > 0).length))
                        color: Colors.textBright
                        font.pixelSize: 13
                        font.family: Fonts.family
                        font.weight: Font.Medium
                    }

                    Text {
                        visible: root.view !== "day"
                        text: row.trend.filter(s => s > 0).length + " of " + row.trend.length + " days"
                        color: Colors.textMuted
                        font.pixelSize: 10
                        font.family: Fonts.family
                    }
                }
            }

            ColumnLayout {
                visible: row.parts.length > 0
                Layout.fillWidth: true
                Layout.leftMargin: 14
                Layout.rightMargin: 14
                spacing: 6

                Text {
                    text: root.agg.details[row.modelData.id] && /wezterm/.test(row.modelData.id) ? "Programs" : "Sites"
                    color: Colors.textMuted
                    font.pixelSize: 10
                    font.family: Fonts.family
                }

                Repeater {
                    model: row.parts.slice(0, 8)

                    ColumnLayout {
                        id: part

                        required property var modelData

                        readonly property var category: ScreenTime.categoryOf(modelData.rest ? row.info.category : ScreenTime.categoryFor(row.modelData.id, modelData.label))

                        Layout.fillWidth: true
                        spacing: 6

                        RowLayout {
                            Layout.fillWidth: true
                            spacing: 12

                            Text {
                                Layout.preferredWidth: 200
                                text: part.modelData.label
                                color: part.modelData.rest ? Colors.textMuted : Colors.text
                                font.pixelSize: 11
                                font.family: Fonts.family
                                elide: Text.ElideRight
                            }

                            Item {
                                Layout.preferredWidth: 120
                                implicitHeight: 22

                                Rectangle {
                                    visible: !part.modelData.rest
                                    width: pill.implicitWidth + 16
                                    height: parent.height
                                    radius: height / 2
                                    color: row.editing === part.modelData.label || pillArea.containsMouse ? Colors.surfaceActive : "transparent"
                                    border.width: 1
                                    border.color: Colors.border

                                    RowLayout {
                                        id: pill

                                        anchors.centerIn: parent
                                        spacing: 5

                                        Rectangle {
                                            implicitWidth: 6
                                            implicitHeight: 6
                                            radius: 3
                                            color: part.category.color
                                        }

                                        Text {
                                            text: part.category.name
                                            color: Colors.textMuted
                                            font.pixelSize: 10
                                            font.family: Fonts.family
                                        }
                                    }

                                    MouseArea {
                                        id: pillArea
                                        anchors.fill: parent
                                        hoverEnabled: true
                                        cursorShape: Qt.PointingHandCursor
                                        onClicked: row.editing = row.editing === part.modelData.label ? "" : part.modelData.label
                                    }
                                }
                            }

                            Rectangle {
                                Layout.fillWidth: true
                                implicitHeight: 3
                                radius: 1.5
                                color: Colors.surfaceActive

                                Rectangle {
                                    width: parent.width * part.modelData.seconds / Math.max(1, row.modelData.seconds)
                                    height: parent.height
                                    radius: 1.5
                                    color: part.modelData.rest ? Colors.outline : part.category.color
                                }
                            }

                            Text {
                                Layout.preferredWidth: 64
                                horizontalAlignment: Text.AlignRight
                                text: ScreenTime.format(part.modelData.seconds)
                                color: Colors.textMuted
                                font.pixelSize: 11
                                font.family: Fonts.family
                                font.features: { "tnum": 1 }
                            }

                            Text {
                                Layout.preferredWidth: 36
                                horizontalAlignment: Text.AlignRight
                                text: Math.round(part.modelData.seconds / Math.max(1, row.modelData.seconds) * 100) + "%"
                                color: Colors.textMuted
                                font.pixelSize: 11
                                font.family: Fonts.family
                                font.features: { "tnum": 1 }
                            }

                            Item { implicitWidth: 16 }
                        }

                        Flow {
                            visible: row.editing === part.modelData.label
                            Layout.fillWidth: true
                            Layout.leftMargin: 212
                            Layout.bottomMargin: 4
                            spacing: 6

                            Repeater {
                                model: ScreenTime.categoryList

                                Chip {
                                    required property var modelData

                                    label: modelData.name
                                    dot: modelData.color
                                    active: part.category.id === modelData.id
                                    onClicked: {
                                        ScreenTime.setLabelCategory(row.modelData.id, part.modelData.label, modelData.id);
                                        row.editing = "";
                                    }
                                }
                            }
                        }
                    }
                }
            }

            Flow {
                Layout.fillWidth: true
                Layout.leftMargin: 14
                Layout.rightMargin: 14
                Layout.bottomMargin: 14
                spacing: 6

                Text {
                    height: 26
                    rightPadding: 4
                    verticalAlignment: Text.AlignVCenter
                    text: "App category"
                    color: Colors.textMuted
                    font.pixelSize: 11
                    font.family: Fonts.family
                }

                Repeater {
                    model: ScreenTime.categoryList

                    Chip {
                        required property var modelData

                        label: modelData.name
                        dot: modelData.color
                        active: row.info.category === modelData.id
                        onClicked: ScreenTime.setCategory(row.modelData.id, modelData.id)
                    }
                }
            }
        }
    }

    component Chip: Rectangle {
        id: chip

        property string label: ""
        property color dot: "transparent"
        property bool active: false
        property bool dimmed: false
        property bool closable: false

        signal clicked

        implicitWidth: chipRow.implicitWidth + 20
        implicitHeight: 26
        radius: 13
        color: active ? Colors.surfaceActive : chipArea.containsMouse && enabled ? Colors.surfaceActive : "transparent"
        border.width: active ? 0 : 1
        border.color: Colors.border
        opacity: dimmed ? 0.5 : 1

        Behavior on color {
            ColorAnimation { duration: 120 }
        }

        RowLayout {
            id: chipRow

            anchors.centerIn: parent
            spacing: 6

            Rectangle {
                implicitWidth: 7
                implicitHeight: 7
                radius: 3.5
                color: chip.dot
            }

            Text {
                text: chip.label
                color: chip.active ? Colors.textBright : Colors.text
                font.pixelSize: 11
                font.family: Fonts.family
            }

            MaterialIcon {
                visible: chip.closable
                text: "close"
                size: 12
                color: Colors.textMuted
            }
        }

        MouseArea {
            id: chipArea
            anchors.fill: parent
            enabled: chip.enabled
            hoverEnabled: true
            cursorShape: Qt.PointingHandCursor
            onClicked: chip.clicked()
        }
    }

    component Stat: Rectangle {
        id: stat

        property string label: ""
        property string value: ""
        property string note: ""

        Layout.fillWidth: true
        Layout.preferredWidth: 1
        implicitHeight: 84
        radius: 12
        color: Colors.surface

        ColumnLayout {
            anchors.fill: parent
            anchors.margins: 14
            spacing: 3

            Text {
                text: stat.label
                color: Colors.textMuted
                font.pixelSize: 11
                font.family: Fonts.family
            }

            Text {
                Layout.fillWidth: true
                text: stat.value
                color: Colors.textBright
                font.pixelSize: 20
                font.family: Fonts.family
                font.weight: Font.Medium
                elide: Text.ElideRight
            }

            Text {
                Layout.fillWidth: true
                text: stat.note
                color: Colors.textMuted
                font.pixelSize: 10
                font.family: Fonts.family
                elide: Text.ElideRight
            }
        }
    }

    component TextButton: Rectangle {
        id: tb

        property string text: ""

        signal clicked

        implicitWidth: tbLabel.implicitWidth + 24
        implicitHeight: 30
        radius: 8
        color: tbArea.containsMouse ? Colors.surfaceActive : Colors.surface

        Behavior on color {
            ColorAnimation { duration: 140 }
        }

        Text {
            id: tbLabel
            anchors.centerIn: parent
            text: tb.text
            color: Colors.text
            font.pixelSize: 12
            font.family: Fonts.family
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
