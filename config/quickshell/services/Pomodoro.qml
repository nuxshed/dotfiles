pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io
import "../config"

Singleton {
    id: root

    readonly property int grace: 10
    readonly property int longEvery: 4
    readonly property var labels: ({ none: "Focus", focus: "Focus", paused: "Paused", break: "Break", long: "Long break" })

    property var targets: ({ focus: 25 * 60, short: 5 * 60, long: 15 * 60 })
    property var session: null
    property var history: []
    property real now: Date.now()
    property bool appOpen: false
    property bool ready: false

    readonly property bool active: session !== null
    readonly property var current: active ? session.segments[session.segments.length - 1] : null
    readonly property string status: !active ? "none" : current.kind === "pause" ? "paused" : current.kind
    readonly property bool isLong: status === "break" && !!current.long
    readonly property string label: labels[isLong ? "long" : status]
    readonly property int breaks: active ? session.segments.filter(s => s.kind === "break").length : 0
    readonly property int block: breaks + 1
    readonly property bool nextLong: (breaks + 1) % longEvery === 0

    readonly property real elapsed: {
        if (!active)
            return 0;
        if (status === "break")
            return (now - current.start) / 1000;
        const segs = session.segments;
        let from = segs.length - 1;
        while (from > 0 && segs[from - 1].kind !== "break")
            from--;
        let ms = 0;
        for (let i = from; i < segs.length; i++)
            if (segs[i].kind === "focus")
                ms += (segs[i].end ?? now) - segs[i].start;
        return ms / 1000;
    }
    readonly property real target: active ? session.target : targets.focus
    readonly property real remaining: target - elapsed
    readonly property bool overtime: active && remaining < 0
    readonly property real progress: active ? Math.min(1, elapsed / Math.max(1, target)) : 0
    readonly property real sessionElapsed: active ? (now - session.start) / 1000 : 0
    readonly property string display: (overtime ? "+" : "") + clock(Math.abs(remaining))

    function clock(seconds: real): string {
        const s = Math.floor(seconds);
        const h = Math.floor(s / 3600);
        const mm = String(Math.floor(s % 3600 / 60)).padStart(2, "0");
        const ss = String(s % 60).padStart(2, "0");
        return h > 0 ? h + ":" + mm + ":" + ss : mm + ":" + ss;
    }

    function duration(seconds: real): string {
        if (seconds > 0 && seconds < 60)
            return Math.round(seconds) + "s";
        const m = Math.round(seconds / 60);
        return m >= 60 ? Math.floor(m / 60) + "h " + String(m % 60).padStart(2, "0") + "m" : m + "m";
    }

    function commit(next: var): void {
        root.now = Date.now();
        root.session = next;
        root.persist();
    }

    function push(kind: string, target: real, extra: var, keep: bool): void {
        const t = Date.now();
        const segs = root.session.segments.map(s => s.end === null ? Object.assign({}, s, { end: t }) : s);
        segs.push(Object.assign({ kind, start: t, end: null }, extra ?? {}));
        root.commit(Object.assign({}, root.session, { segments: segs, target, notified: keep ? root.session.notified : false }));
    }

    function start(): void {
        if (root.active)
            return;
        const t = Date.now();
        root.commit({ start: t, target: root.targets.focus, notified: false, segments: [{ kind: "focus", start: t, end: null }] });
    }

    function pause(): void {
        if (root.status === "focus")
            root.push("pause", root.session.target, null, true);
    }

    function resume(): void {
        if (root.status === "paused")
            root.push("focus", root.session.target + root.grace, null, true);
    }

    function takeBreak(): void {
        if (root.status !== "focus" && root.status !== "paused")
            return;
        const long = root.nextLong;
        root.push("break", long ? root.targets.long : root.targets.short, { long }, false);
    }

    function focus(): void {
        if (root.status === "break")
            root.push("focus", root.targets.focus, null, false);
    }

    function toggle(): void {
        if (root.status === "none")
            root.start();
        else if (root.status === "focus")
            root.pause();
        else if (root.status === "paused")
            root.resume();
    }

    function swap(): void {
        if (root.status === "break")
            root.focus();
        else
            root.takeBreak();
    }

    function extend(seconds: int): void {
        if (root.active)
            root.commit(Object.assign({}, root.session, { target: Math.max(60, root.session.target + seconds), notified: false }));
    }

    function end(): void {
        if (!root.active)
            return;
        const t = Date.now();
        const done = { id: root.session.start, start: root.session.start, end: t, segments: root.session.segments.map(s => s.end === null ? Object.assign({}, s, { end: t }) : s).filter(s => s.end - s.start >= 1000).map(s => ({ kind: s.kind, start: s.start, end: s.end, long: s.long })) };
        if (done.segments.some(s => s.kind === "focus")) {
            root.history = root.history.concat([done]);
            root.saveHistory();
        }
        root.commit(null);
    }

    function saveHistory(): void {
        historyFile.setText(JSON.stringify({ sessions: root.history }));
    }

    function keyOf(s: var): real {
        return s.id ?? s.start;
    }

    function removeSession(id: real): void {
        root.history = root.history.filter(s => root.keyOf(s) !== id);
        root.saveHistory();
    }

    function editSession(id: real, edit: var): void {
        root.history = [].concat(...root.history.map(s => {
            if (root.keyOf(s) !== id)
                return [s];
            const segs = edit(s.segments.map(x => Object.assign({}, x))).filter(x => x.end - x.start >= 1000).sort((a, b) => a.start - b.start);
            for (let i = 0; i + 1 < segs.length; i++)
                segs[i].end = Math.min(segs[i].end, segs[i + 1].start);
            const clean = segs.filter(x => x.end - x.start >= 1000);
            return clean.length === 0 ? [] : [{ id: root.keyOf(s), start: clean[0].start, end: clean[clean.length - 1].end, segments: clean }];
        }));
        root.saveHistory();
    }

    function setSegmentTime(id: real, index: int, field: string, ms: real): void {
        root.editSession(id, segs => {
            const old = segs[index][field];
            segs[index][field] = ms;
            if (field === "end" && index + 1 < segs.length && segs[index + 1].start === old)
                segs[index + 1].start = ms;
            if (field === "start" && index > 0 && segs[index - 1].end === old)
                segs[index - 1].end = ms;
            return segs;
        });
    }

    function setSegmentKind(id: real, index: int, kind: string): void {
        root.editSession(id, segs => {
            segs[index].kind = kind;
            segs[index].long = kind === "break" ? segs[index].long : undefined;
            return segs;
        });
    }

    function removeSegment(id: real, index: int): void {
        root.editSession(id, segs => segs.filter((_, i) => i !== index));
    }

    function setTarget(kind: string, seconds: int): void {
        const next = Object.assign({}, root.targets);
        next[kind] = Math.max(60, Math.min(180 * 60, seconds));
        root.targets = next;
        root.persist();
    }

    function historyBetween(a: real, b: real): var {
        return root.history.filter(s => s.start < b && s.end > a);
    }

    function sessionsBetween(a: real, b: real): var {
        const all = root.active ? root.history.concat([Object.assign({}, root.session, { end: root.now, live: true, segments: root.session.segments.map(s => s.end === null ? Object.assign({}, s, { end: root.now }) : s) })]) : root.history;
        return all.filter(s => s.start < b && s.end > a);
    }

    function totals(sessions: var, a: real, b: real): var {
        const out = { focus: 0, break: 0, pause: 0, longest: 0, sessions: sessions.length };
        for (const s of sessions) {
            let run = 0;
            for (const seg of s.segments) {
                const ms = Math.max(0, Math.min(seg.end, b) - Math.max(seg.start, a));
                out[seg.kind] += ms / 1000;
                if (seg.kind === "focus")
                    run += ms / 1000;
                else if (seg.kind === "break")
                    run = 0;
                out.longest = Math.max(out.longest, run);
            }
        }
        return out;
    }

    function tick(): void {
        root.now = Date.now();
        if (root.active && !root.session.notified && root.remaining <= 0 && root.status !== "paused") {
            Quickshell.execDetached(["notify-send", "-a", "Focus", "-i", "tomato",
                root.status === "break" ? "Break's up" : "Focus target reached",
                root.status === "break" ? "Switch back to focus when you're ready" : "Take a " + (root.nextLong ? "long break" : "break") + " when you're ready"]);
            root.session = Object.assign({}, root.session, { notified: true });
            root.persist();
        }
    }

    function persist(): void {
        if (root.ready)
            stateFile.setText(JSON.stringify({ targets: root.targets, session: root.session }));
    }

    Timer {
        interval: 1000
        running: root.active
        repeat: true
        triggeredOnStart: true
        onTriggered: root.tick()
    }

    FileView {
        id: stateFile
        path: SpotlightConfig.stateDir + "/pomodoro.json"
        printErrors: false
        onLoaded: {
            try {
                const data = JSON.parse(text());
                if (data.targets?.focus)
                    root.targets = data.targets;
                if (data.session?.segments?.length)
                    root.session = data.session;
            } catch (e) {}
            root.ready = true;
        }
        onLoadFailed: root.ready = true
    }

    FileView {
        id: historyFile
        path: SpotlightConfig.stateDir + "/focus-history.json"
        printErrors: false
        onLoaded: {
            try {
                root.history = JSON.parse(text()).sessions ?? [];
            } catch (e) {}
        }
    }
}
