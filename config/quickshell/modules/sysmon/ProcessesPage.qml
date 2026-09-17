pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import "../../components"
import "../../config"
import "../../services"
import "../preview"
import "../files"

ColumnLayout {
    id: root

    property string sortKey: "cpu"
    property bool sortAsc: false
    property string filter: ""
    property bool grouped: true
    property var expanded: ({})
    property string selectedGroup: ""
    readonly property var columns: [
        { key: "name", name: "Name", width: 0, align: Text.AlignLeft },
        { key: "pid", name: "PID", width: 76, align: Text.AlignRight },
        { key: "user", name: "User", width: 100, align: Text.AlignLeft },
        { key: "cpu", name: "CPU", width: 72, align: Text.AlignRight },
        { key: "mem", name: "Memory", width: 96, align: Text.AlignRight },
        { key: "threads", name: "Threads", width: 66, align: Text.AlignRight }
    ]
    readonly property var selectedPids: {
        if (SysMon.selectedPid > 0)
            return SysMon.procs.some(p => p.pid === SysMon.selectedPid) ? [SysMon.selectedPid] : [];
        if (root.selectedGroup.length > 0)
            return SysMon.procs.filter(p => p.name === root.selectedGroup).map(p => p.pid);
        return [];
    }

    function sortBy(key: string): void {
        if (root.sortKey === key)
            root.sortAsc = !root.sortAsc;
        else {
            root.sortKey = key;
            root.sortAsc = key === "name" || key === "user";
        }
        root.sync();
    }

    function toggle(name: string): void {
        const e = Object.assign({}, root.expanded);
        if (e[name])
            delete e[name];
        else
            e[name] = true;
        root.expanded = e;
        root.sync();
    }

    function select(row: var): void {
        if (row.group) {
            const same = root.selectedGroup === row.name;
            root.selectedGroup = same ? "" : row.name;
            SysMon.selectedPid = 0;
        } else {
            const same = SysMon.selectedPid === row.pid;
            SysMon.selectedPid = same ? 0 : row.pid;
            root.selectedGroup = "";
        }
    }

    function sync(): void {
        const q = root.filter.toLowerCase();
        const key = root.sortKey;
        const dir = root.sortAsc ? 1 : -1;
        const cmp = (a, b) => {
            const x = a[key], y = b[key];
            const c = typeof x === "string" ? x.localeCompare(y) : x - y;
            return c !== 0 ? c * dir : a.pid - b.pid;
        };
        const matches = SysMon.procs.filter(p => q.length === 0 || p.name.toLowerCase().includes(q) || p.user.toLowerCase().includes(q) || String(p.pid) === q);
        let rows = [];
        if (root.grouped) {
            const groups = {};
            for (const p of matches) {
                const g = groups[p.name] ?? (groups[p.name] = { name: p.name, user: p.user, pid: p.pid, cpu: 0, mem: 0, threads: 0, pstate: p.pstate, count: 0, members: [] });
                g.cpu += p.cpu;
                g.mem += p.mem;
                g.threads += p.threads;
                g.count++;
                g.pid = Math.min(g.pid, p.pid);
                g.members.push(p);
                if (p.user !== g.user)
                    g.user = "multiple";
            }
            const list = Object.values(groups).sort(cmp);
            for (const g of list) {
                const open = g.count > 1 && root.expanded[g.name] === true;
                rows.push({ pid: g.pid, name: g.name, user: g.user, cpu: g.cpu, mem: g.mem, threads: g.threads, pstate: g.pstate, count: g.count, group: g.count > 1, child: false, open });
                if (open)
                    for (const p of g.members.sort(cmp))
                        rows.push(Object.assign({ count: 1, group: false, child: true, open: false }, p));
            }
        } else {
            rows = matches.sort(cmp).map(p => Object.assign({ count: 1, group: false, child: false, open: false }, p));
        }
        for (let i = 0; i < rows.length; i++) {
            if (i < procModel.count)
                procModel.set(i, rows[i]);
            else
                procModel.append(rows[i]);
        }
        if (procModel.count > rows.length)
            procModel.remove(rows.length, procModel.count - rows.length);
    }

    spacing: 12

    Connections {
        target: SysMon
        function onProcsChanged() { root.sync(); }
    }

    onFilterChanged: root.sync()
    onGroupedChanged: root.sync()

    ListModel { id: procModel }

    RowLayout {
        Layout.fillWidth: true
        Layout.fillHeight: false
        spacing: 8

        PageHeader {
            title: "Processes"
            subtitle: `${SysMon.procCount} processes · ${SysMon.threadCount} threads`
        }

        Field {
            Layout.preferredWidth: 190
            implicitHeight: 32
            icon: "search"
            placeholder: "Filter"
            onTextChanged: root.filter = text
            onEscaped: text = ""
        }

        PreviewButton {
            implicitWidth: 32
            implicitHeight: 32
            icon: "device_hub"
            active: root.grouped
            onClicked: root.grouped = !root.grouped
        }

        TextButton {
            text: root.selectedPids.length > 1 ? `End ${root.selectedPids.length} tasks` : "End task"
            implicitHeight: 32
            enabled: root.selectedPids.length > 0
            onClicked: SysMon.endProcesses(root.selectedPids, false)
        }

        TextButton {
            text: "Kill"
            implicitHeight: 32
            primary: true
            danger: true
            enabled: root.selectedPids.length > 0
            onClicked: SysMon.endProcesses(root.selectedPids, true)
        }
    }

    Rectangle {
        Layout.fillWidth: true
        Layout.fillHeight: true
        radius: 10
        color: Colors.surfaceActive
        border.width: 1
        border.color: Colors.outline
        clip: true

        ColumnLayout {
            anchors.fill: parent
            spacing: 0

            RowLayout {
                Layout.fillWidth: true
                Layout.fillHeight: false
                Layout.preferredHeight: 36
                Layout.leftMargin: 14
                Layout.rightMargin: 14
                spacing: 12

                Repeater {
                    model: root.columns

                    Item {
                        id: header
                        required property var modelData
                        readonly property bool active: root.sortKey === modelData.key
                        Layout.fillWidth: modelData.width === 0
                        Layout.preferredWidth: modelData.width === 0 ? -1 : modelData.width
                        Layout.fillHeight: true

                        Row {
                            anchors.verticalCenter: parent.verticalCenter
                            anchors.left: header.modelData.align === Text.AlignLeft ? parent.left : undefined
                            anchors.right: header.modelData.align === Text.AlignRight ? parent.right : undefined
                            spacing: 2
                            layoutDirection: header.modelData.align === Text.AlignRight ? Qt.RightToLeft : Qt.LeftToRight

                            Text {
                                text: header.modelData.name
                                color: header.active ? Colors.textBright : Colors.textMuted
                                font.pixelSize: 11
                                font.family: Fonts.family
                                font.weight: header.active ? Font.Medium : Font.Normal
                                anchors.verticalCenter: parent.verticalCenter
                            }
                            MaterialIcon {
                                visible: header.active
                                text: root.sortAsc ? "arrow_upward" : "arrow_downward"
                                size: 12
                                color: Colors.textBright
                                anchors.verticalCenter: parent.verticalCenter
                            }
                        }

                        MouseArea {
                            anchors.fill: parent
                            cursorShape: Qt.PointingHandCursor
                            onClicked: root.sortBy(header.modelData.key)
                        }
                    }
                }
            }

            Rectangle { Layout.fillWidth: true; Layout.preferredHeight: 1; color: Colors.subtle }

            ListView {
                id: list
                Layout.fillWidth: true
                Layout.fillHeight: true
                model: procModel
                clip: true
                boundsBehavior: Flickable.StopAtBounds
                reuseItems: true

                delegate: Rectangle {
                    id: row
                    required property int pid
                    required property string name
                    required property string user
                    required property real cpu
                    required property real mem
                    required property int threads
                    required property string pstate
                    required property int count
                    required property bool group
                    required property bool child
                    required property bool open
                    readonly property bool selected: group ? root.selectedGroup === name : SysMon.selectedPid === pid

                    width: list.width
                    height: 28
                    color: selected ? Colors.subtle : rowMouse.containsMouse ? Qt.alpha(Colors.subtle, 0.5) : child ? Qt.alpha(Colors.surface, 0.5) : "transparent"

                    MouseArea {
                        id: rowMouse
                        anchors.fill: parent
                        hoverEnabled: true
                        onClicked: root.select(row)
                        onDoubleClicked: if (row.group) root.toggle(row.name)
                    }

                    RowLayout {
                        anchors.fill: parent
                        anchors.leftMargin: 14
                        anchors.rightMargin: 14
                        spacing: 12

                        Item {
                            Layout.fillWidth: true
                            Layout.fillHeight: true

                            Row {
                                anchors.left: parent.left
                                anchors.leftMargin: row.child ? 16 : 0
                                anchors.right: parent.right
                                anchors.verticalCenter: parent.verticalCenter
                                spacing: 8

                                Text {
                                    id: nameText
                                    width: Math.min(implicitWidth, parent.width - (row.group ? badge.width + 8 : 0))
                                    text: row.name
                                    color: row.pstate === "Z" ? Colors.red : row.child ? Colors.textDimmed : row.selected ? Colors.textBright : Colors.text
                                    font.pixelSize: 12
                                    font.family: Fonts.family
                                    elide: Text.ElideRight
                                    anchors.verticalCenter: parent.verticalCenter
                                }
                                Rectangle {
                                    id: badge
                                    visible: row.group
                                    width: badgeText.width + 12
                                    height: 16
                                    radius: 8
                                    color: badgeMouse.containsMouse || row.open ? Colors.outline : Colors.subtle
                                    anchors.verticalCenter: parent.verticalCenter

                                    Text {
                                        id: badgeText
                                        anchors.centerIn: parent
                                        text: row.count
                                        color: row.open ? Colors.textBright : Colors.textDimmed
                                        font.pixelSize: 10
                                        font.family: Fonts.family
                                    }

                                    MouseArea {
                                        id: badgeMouse
                                        anchors.fill: parent
                                        hoverEnabled: true
                                        cursorShape: Qt.PointingHandCursor
                                        onClicked: root.toggle(row.name)
                                    }
                                }
                            }
                        }
                        Text {
                            Layout.preferredWidth: 76
                            text: row.group ? "" : row.pid
                            color: Colors.textMuted
                            font.pixelSize: 11
                            font.family: Fonts.family
                            horizontalAlignment: Text.AlignRight
                        }
                        Text {
                            Layout.preferredWidth: 100
                            text: row.user
                            color: Colors.textDimmed
                            font.pixelSize: 11
                            font.family: Fonts.family
                            elide: Text.ElideRight
                        }
                        Text {
                            Layout.preferredWidth: 72
                            text: row.cpu < 0.05 ? "0%" : `${row.cpu.toFixed(1)}%`
                            color: row.cpu >= 10 ? Colors.yellow : row.cpu >= 1 ? Colors.textBright : Colors.textDimmed
                            font.pixelSize: 11
                            font.family: Fonts.family
                            horizontalAlignment: Text.AlignRight
                        }
                        Text {
                            Layout.preferredWidth: 96
                            text: row.mem === 0 ? "—" : SysMon.fmtBytes(row.mem, 1)
                            color: row.mem >= 1073741824 ? Colors.yellow : Colors.textDimmed
                            font.pixelSize: 11
                            font.family: Fonts.family
                            horizontalAlignment: Text.AlignRight
                        }
                        Text {
                            Layout.preferredWidth: 66
                            text: row.threads
                            color: Colors.textMuted
                            font.pixelSize: 11
                            font.family: Fonts.family
                            horizontalAlignment: Text.AlignRight
                        }
                    }


                }

                Text {
                    anchors.centerIn: parent
                    visible: procModel.count === 0
                    text: SysMon.procs.length === 0 ? "Loading…" : "No matching processes"
                    color: Colors.textMuted
                    font.pixelSize: 12
                    font.family: Fonts.family
                }
            }
        }
    }
}
