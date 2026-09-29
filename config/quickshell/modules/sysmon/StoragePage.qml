pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import Quickshell
import "../../config"
import "../../services"
import "../preview"
import "../files"

Item {
    id: page

    Confirm { id: confirm; anchors.fill: parent; z: 50 }

    ColumnLayout {
    id: root
    anchors.fill: parent

    readonly property string home: Quickshell.env("HOME")
    readonly property var crumbs: {
        const p = SysMon.duPath;
        if (p.length === 0)
            return [];
        const out = [];
        if (p.startsWith(root.home)) {
            out.push({ name: "~", path: root.home });
            for (const seg of p.slice(root.home.length).split("/").filter(s => s.length > 0))
                out.push({ name: seg, path: out[out.length - 1].path + "/" + seg });
        } else {
            out.push({ name: "/", path: "/" });
            for (const seg of p.split("/").filter(s => s.length > 0))
                out.push({ name: seg, path: (out[out.length - 1].path === "/" ? "" : out[out.length - 1].path) + "/" + seg });
        }
        if (SysMon.duFiles)
            out.push({ name: "Files", path: p, files: true });
        return out;
    }

    function context(entry: var, x: real, y: real): void {
        const items = [];
        if (entry.dir)
            items.push({ label: "Open", icon: "folder_open", act: () => SysMon.showDir(entry.path) });
        if (entry.file === undefined && !entry.dir)
            items.push({ label: "Show files", icon: "subject", act: () => SysMon.showFiles(entry.path) });
        items.push({ label: "Open in Files", icon: "folder", act: () => Files.browse(entry.dir ? entry.path : SysMon.duPath) });
        if (entry.dir || entry.file) {
            items.push({ label: "Move to trash", icon: "delete", divider: true, act: () => confirm.ask(`Move "${entry.name}" to trash?`, `${SysMon.fmtBytes(entry.size, 1)} will be moved to the trash. You can restore it from there.`, "Trash", () => SysMon.remove(entry.path, false)) });
            items.push({ label: "Delete permanently", icon: "delete_forever", danger: true, act: () => confirm.ask(`Delete "${entry.name}" permanently?`, `${SysMon.fmtBytes(entry.size, 1)} will be deleted. This cannot be undone.`, "Delete", () => SysMon.remove(entry.path, true)) });
        }
        menu.items = items;
        menu.popup(x, y, mapBox.width, mapBox.height);
    }
    readonly property string age: {
        const s = (root.now - SysMon.duScanned) / 1000;
        if (s < 90)
            return "just now";
        if (s < 5400)
            return `${Math.round(s / 60)} min ago`;
        if (s < 172800)
            return `${Math.round(s / 3600)} h ago`;
        return `${Math.round(s / 86400)} d ago`;
    }
    property real now: Date.now()
    readonly property var hoveredEntry: map.hovered >= 0 ? SysMon.duEntries[map.hovered] : null

    spacing: 12

    Timer {
        interval: 30000
        running: true
        repeat: true
        onTriggered: root.now = Date.now()
    }

    Component.onCompleted: if (SysMon.duPath.length === 0) SysMon.showDir(root.home)

    RowLayout {
        Layout.fillWidth: true
        Layout.fillHeight: false
        spacing: 8

        PageHeader {
            title: "Storage"
            subtitle: SysMon.filesystems.length > 0 ? `${SysMon.filesystems.length} mounted filesystem${SysMon.filesystems.length === 1 ? "" : "s"}` : ""
        }

        TextButton {
            text: "Maintenance"
            implicitHeight: 32
            onClicked: SysMon.openMaintenance()
        }
    }

    Panel {
        Layout.fillWidth: true
        Layout.fillHeight: false
        title: "Filesystems"

        Repeater {
            model: SysMon.filesystems

            RowLayout {
                id: fsRow
                required property var modelData
                readonly property real frac: modelData.size > 0 ? modelData.used / modelData.size : 0
                Layout.fillWidth: true
                spacing: 12

                Text {
                    Layout.preferredWidth: 120
                    text: fsRow.modelData.mount
                    color: Colors.text
                    font.pixelSize: 11
                    font.family: Fonts.family
                    elide: Text.ElideRight
                }
                Text {
                    Layout.preferredWidth: 150
                    text: `${fsRow.modelData.device} · ${fsRow.modelData.type}`
                    color: Colors.textMuted
                    font.pixelSize: 10
                    font.family: Fonts.family
                    elide: Text.ElideRight
                }
                Bar {
                    Layout.fillWidth: true
                    value: fsRow.frac
                    accent: fsRow.frac > 0.9 ? Colors.red : fsRow.frac > 0.75 ? Colors.yellow : Colors.primary
                }
                Text {
                    Layout.preferredWidth: 150
                    text: `${SysMon.fmtBytes(fsRow.modelData.used, 1)} of ${SysMon.fmtBytes(fsRow.modelData.size, 1)}`
                    color: Colors.textDimmed
                    font.pixelSize: 11
                    font.family: Fonts.family
                    horizontalAlignment: Text.AlignRight
                }
                Text {
                    Layout.preferredWidth: 34
                    text: `${(fsRow.frac * 100).toFixed(0)}%`
                    color: Colors.text
                    font.pixelSize: 11
                    font.family: Fonts.family
                    horizontalAlignment: Text.AlignRight
                }

                MouseArea {
                    anchors.fill: parent
                    cursorShape: Qt.PointingHandCursor
                    onClicked: SysMon.showDir(fsRow.modelData.mount)
                }
            }
        }
    }

    Rectangle {
        id: mapBox
        Layout.fillWidth: true
        Layout.fillHeight: true
        radius: 10
        color: Colors.surfaceActive
        border.width: 1
        border.color: Colors.outline

        MouseArea {
            anchors.fill: parent
            visible: menu.shown
            z: 19
            acceptedButtons: Qt.AllButtons
            onClicked: menu.shown = false
        }
        Menu { id: menu }

        ColumnLayout {
            anchors.fill: parent
            anchors.margins: 14
            spacing: 10

            RowLayout {
                Layout.fillWidth: true
                Layout.fillHeight: false
                spacing: 4

                Repeater {
                    model: root.crumbs

                    Row {
                        id: crumb
                        required property int index
                        required property var modelData
                        readonly property bool last: index === root.crumbs.length - 1
                        spacing: 4

                        Text {
                            visible: crumb.index > 0
                            text: "/"
                            color: Colors.textMuted
                            font.pixelSize: 12
                            font.family: Fonts.family
                            anchors.verticalCenter: parent.verticalCenter
                        }
                        Text {
                            text: crumb.modelData.name
                            color: crumb.last ? Colors.textBright : crumbMouse.containsMouse ? Colors.text : Colors.textDimmed
                            font.pixelSize: 12
                            font.family: Fonts.family
                            font.weight: crumb.last ? Font.Medium : Font.Normal
                            anchors.verticalCenter: parent.verticalCenter

                            MouseArea {
                                id: crumbMouse
                                anchors.fill: parent
                                enabled: !crumb.last
                                hoverEnabled: true
                                cursorShape: Qt.PointingHandCursor
                                onClicked: SysMon.showDir(crumb.modelData.path)
                            }
                        }
                    }
                }

                Text {
                    Layout.fillWidth: true
                    horizontalAlignment: Text.AlignRight
                    elide: Text.ElideLeft
                    text: {
                        if (SysMon.scanning)
                            return "Scanning…";
                        if (root.hoveredEntry)
                            return `${root.hoveredEntry.name} · ${SysMon.fmtBytes(root.hoveredEntry.size, 1)} · ${SysMon.duTotal > 0 ? (root.hoveredEntry.size / SysMon.duTotal * 100).toFixed(1) : 0}%`;
                        if (SysMon.duFiles)
                            return `${SysMon.fmtBytes(SysMon.duTotal, 1)} of files · right-click to delete`;
                        return SysMon.duTotal > 0 ? `${SysMon.fmtBytes(SysMon.duTotal, 1)} in ${SysMon.duEntries.length} items · scanned ${root.age}` : "";
                    }
                    color: Colors.textMuted
                    font.pixelSize: 11
                    font.family: Fonts.family
                }

                PreviewButton {
                    implicitWidth: 28
                    implicitHeight: 28
                    icon: "arrow_upward"
                    enabled: root.crumbs.length > 1 && !SysMon.scanning
                    onClicked: SysMon.showDir(SysMon.duFiles ? SysMon.duPath : root.crumbs[root.crumbs.length - 2].path)
                }
                PreviewButton {
                    implicitWidth: 28
                    implicitHeight: 28
                    icon: "refresh"
                    enabled: !SysMon.scanning
                    onClicked: SysMon.duFiles ? SysMon.showFiles(SysMon.duPath) : SysMon.scanDir(SysMon.duPath)
                }
                PreviewButton {
                    implicitWidth: 28
                    implicitHeight: 28
                    icon: "folder_open"
                    onClicked: Files.browse(SysMon.duPath)
                }
            }

            Treemap {
                id: map
                Layout.fillWidth: true
                Layout.fillHeight: true
                entries: SysMon.duEntries
                opacity: SysMon.scanning ? 0.4 : 1
                onActivated: entry => {
                    if (entry.dir)
                        SysMon.showDir(entry.path);
                    else if (entry.file === undefined)
                        SysMon.showFiles(entry.path);
                }
                onContextRequested: (entry, x, y) => root.context(entry, x + 14, y + map.y + 14)

                Behavior on opacity { NumberAnimation { duration: 150 } }

                Text {
                    anchors.centerIn: parent
                    visible: SysMon.scanning && SysMon.duEntries.length === 0
                    text: `Scanning ${SysMon.duPath}…`
                    color: Colors.textMuted
                    font.pixelSize: 12
                    font.family: Fonts.family
                }
                Text {
                    anchors.centerIn: parent
                    visible: !SysMon.scanning && SysMon.duEntries.length === 0
                    text: "Empty or unreadable"
                    color: Colors.textMuted
                    font.pixelSize: 12
                    font.family: Fonts.family
                }
            }
        }
    }
}
}
