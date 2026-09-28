pragma Singleton

import QtQuick
import Quickshell

Singleton {
    id: root

    property var items: []
    property int counter: 0
    property real now: Date.now()

    readonly property var running: items.filter(t => t.running)
    readonly property var pinned: items.filter(t => t.pinned)
    readonly property var floating: items.filter(t => !t.pinned)
    readonly property bool any: floating.length > 0

    function elapsed(t: var): real {
        return t.elapsed + (t.running ? root.now - t.since : 0);
    }

    function remaining(t: var): real {
        return Math.max(0, t.duration - root.elapsed(t));
    }

    function clock(ms: real, showHours: bool): string {
        const s = Math.floor(ms / 1000);
        const p = n => String(n).padStart(2, "0");
        const h = Math.floor(s / 3600);
        const m = Math.floor(s % 3600 / 60);
        return (h > 0 || showHours ? h + ":" + p(m) : String(m)) + ":" + p(s % 60);
    }

    function stopwatch(): void {
        root.items = root.items.concat([{ uid: ++root.counter, kind: "stopwatch", duration: 0, elapsed: 0, since: Date.now(), running: true, done: false, pinned: true }]);
    }

    function timer(minutes: real, label): void {
        root.items = root.items.concat([{ uid: ++root.counter, kind: "timer", label: label ?? "", duration: minutes * 60000, elapsed: 0, since: Date.now(), running: true, done: false, pinned: true }]);
    }

    function update(uid: int, patch: var): void {
        root.items = root.items.map(t => t.uid === uid ? Object.assign({}, t, patch) : t);
    }

    function toggle(uid: int): void {
        const t = root.items.find(t => t.uid === uid);
        if (!t || t.done)
            return;
        if (t.running)
            root.update(uid, { running: false, elapsed: root.elapsed(t) });
        else
            root.update(uid, { running: true, since: Date.now() });
    }

    function reset(uid: int): void {
        root.update(uid, { elapsed: 0, since: Date.now(), running: true, done: false });
    }

    function pin(uid: int, value: bool): void {
        root.update(uid, { pinned: value });
    }

    function remove(uid: int): void {
        root.items = root.items.filter(t => t.uid !== uid);
    }

    Timer {
        interval: 200
        running: root.running.length > 0
        repeat: true
        onTriggered: {
            root.now = Date.now();
            for (const t of root.running)
                if (t.kind === "timer" && !t.done && root.remaining(t) <= 0) {
                    root.update(t.uid, { running: false, elapsed: t.duration, done: true });
                    Quickshell.execDetached(["notify-send", "-a", "Timer", "-i", "alarm-timer", t.label || "Timer done", t.label ? root.clock(t.duration, false) + " timer is up" : root.clock(t.duration, false) + " is up"]);
                }
        }
    }
}
