pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io
import Quickshell.Wayland
import Quickshell.Hyprland
import "../config"

Singleton {
    id: root

    readonly property int idleTimeout: 120
    readonly property int keep: 30

    property var days: ({})
    property string current: ""
    property real since: Date.now()
    property bool ready: false
    property int revision: 0

    readonly property string today: key(new Date())
    readonly property bool paused: idle.isIdle || Lock.locked
    readonly property var todayApps: {
        revision;
        const apps = days[today] ?? {};
        return Object.keys(apps).map(id => ({ id, name: name(id), seconds: apps[id] })).sort((a, b) => b.seconds - a.seconds);
    }
    readonly property int todayTotal: todayApps.reduce((s, a) => s + a.seconds, 0)
    readonly property var week: {
        revision;
        const out = [];
        for (let i = 6; i >= 0; i--) {
            const d = new Date();
            d.setDate(d.getDate() - i);
            const apps = days[key(d)] ?? {};
            out.push({ label: Qt.formatDate(d, "ddd").slice(0, 1), seconds: Object.values(apps).reduce((s, v) => s + v, 0), today: i === 0 });
        }
        return out;
    }

    function key(d: date): string {
        return Qt.formatDate(d, "yyyy-MM-dd");
    }

    function name(id: string): string {
        const entry = DesktopEntries.heuristicLookup(id);
        return entry?.name ?? id;
    }

    function format(seconds: int): string {
        const h = Math.floor(seconds / 3600);
        const m = Math.floor(seconds % 3600 / 60);
        return h > 0 ? `${h}h ${m}m` : `${m}m`;
    }

    function tick(): void {
        const now = Date.now();
        const elapsed = Math.min((now - root.since) / 1000, root.idleTimeout);
        root.since = now;
        if (!root.current || root.paused || elapsed < 1)
            return;
        const day = root.key(new Date());
        const apps = root.days[day] ?? {};
        apps[root.current] = Math.round((apps[root.current] ?? 0) + elapsed);
        root.days[day] = apps;
        root.revision++;
        if (root.ready)
            save.restart();
    }

    function prune(): void {
        const cutoff = new Date();
        cutoff.setDate(cutoff.getDate() - root.keep);
        const min = root.key(cutoff);
        for (const day of Object.keys(root.days))
            if (day < min)
                delete root.days[day];
    }

    function load(text: string): void {
        try {
            root.days = JSON.parse(text) ?? {};
        } catch (e) {
            root.days = {};
        }
        root.prune();
        root.ready = true;
        root.revision++;
    }

    Connections {
        target: Hyprland

        function onActiveToplevelChanged(): void {
            root.tick();
            root.current = Hyprland.activeToplevel?.lastIpcObject?.class ?? "";
        }
    }

    onPausedChanged: root.tick()

    Component.onCompleted: root.current = Hyprland.activeToplevel?.lastIpcObject?.class ?? ""

    IdleMonitor {
        id: idle
        timeout: root.idleTimeout
    }

    Timer {
        interval: 15000
        running: true
        repeat: true
        onTriggered: root.tick()
    }

    FileView {
        id: file
        path: SpotlightConfig.stateDir + "/screentime.json"
        printErrors: false
        onLoaded: root.load(text())
        onLoadFailed: root.load("")
    }

    Timer {
        id: save
        interval: 5000
        onTriggered: file.setText(JSON.stringify(root.days))
    }
}
