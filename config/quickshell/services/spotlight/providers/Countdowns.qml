import QtQuick
import Quickshell
import "../../../services"
import ".."

Provider {
    id: root

    name: "timer"
    label: "Timers"
    prefix: "ti"
    icon: "hourglass_empty"
    keywords: ["timer", "timers", "countdown", "stopwatch", "alarm"]
    weight: 1

    entries: [
        { id: "stopwatch", title: "Start stopwatch", subtitle: "Counts up until you stop it", icon: "timer", keywords: ["stopwatch"], run: () => Timers.stopwatch() },
        { id: "timer", title: "Start a timer", subtitle: "Pick a length, or type “timer 25m”", icon: "hourglass_empty", keywords: ["timer", "countdown"], complete: "ti " }
    ]

    readonly property var unit: /(\d+(?:\.\d+)?)\s*(hours?|hrs?|h|minutes?|mins?|m|seconds?|secs?|s)\b/gi

    function duration(text) {
        const t = text.trim()
        if (!/^(\d+(?:\.\d+)?\s*(hours?|hrs?|h|minutes?|mins?|m|seconds?|secs?|s)\s*(and\s+)?)+$/i.test(t))
            return 0
        let minutes = 0
        const re = new RegExp(root.unit.source, "gi")
        let m
        while ((m = re.exec(t)) !== null) {
            const n = parseFloat(m[1])
            const u = m[2].toLowerCase()
            minutes += u.startsWith("h") ? n * 60 : u.startsWith("s") ? n / 60 : n
        }
        return minutes
    }

    function parse(text, scoped) {
        const q = text.trim().toLowerCase()
        const forms = [
            /^(?:set\s+)?(?:a\s+)?(?:timer|countdown)\s+(?:for\s+)?(.+?)(?:\s+(?:for|to|called|named)\s+(.+))?$/,
            /^(.+?)\s+(?:timer|countdown)(?:\s+(?:for|to)\s+(.+))?$/,
            /^remind\s+me\s+in\s+(.+?)(?:\s+(?:to|about)\s+(.+))?$/,
            /^(?:in\s+)?(.+?)(?:\s+(?:for|to)\s+(.+))?$/
        ]
        for (let i = 0; i < forms.length; i++) {
            if (i === forms.length - 1 && !scoped)
                break
            const m = q.match(forms[i])
            if (!m)
                continue
            const minutes = root.duration(m[1])
            if (minutes > 0)
                return { minutes: minutes, label: m[2] ? text.trim().slice(-m[2].length) : "" }
        }
        return null
    }

    function human(minutes) {
        const s = Math.round(minutes * 60)
        const h = Math.floor(s / 3600)
        const m = Math.floor(s % 3600 / 60)
        const parts = []
        if (h)
            parts.push(h + "h")
        if (m)
            parts.push(m + "m")
        if (s % 60)
            parts.push(s % 60 + "s")
        return parts.join(" ")
    }

    function runningItems() {
        return Timers.items.map((t, i) => {
            const countdown = t.kind === "timer"
            const state = t.done ? "Done" : t.running ? "Running" : "Paused"
            return {
                key: "timer:" + t.uid,
                kind: "timer",
                section: "Running timers",
                icon: countdown ? "hourglass_empty" : "timer",
                title: t.label || (countdown ? root.human(t.duration / 60000) + " timer" : "Stopwatch"),
                subtitle: state,
                badge: Timers.clock(countdown ? Timers.remaining(t) : Timers.elapsed(t), false),
                live: t.running,
                score: 0.9 - i * 0.01,
                pinned: true,
                primary: t.done ? "Dismiss" : t.running ? "Pause" : "Resume",
                activate: () => t.done ? Timers.remove(t.uid) : Timers.toggle(t.uid),
                altActivate: () => Timers.remove(t.uid),
                altHint: "Remove",
                altIcon: "close",
                actions: [
                    { icon: "replay", title: "Restart", run: () => Timers.reset(t.uid) },
                    { icon: "picture_in_picture_alt", title: t.pinned ? "Unpin from screen" : "Pin to screen", run: () => Timers.pin(t.uid, !t.pinned) }
                ]
            }
        })
    }

    property string last: ""
    property bool lastScoped: false

    function search(text, scoped) {
        const q = text.trim()
        root.last = text
        root.lastScoped = !!scoped
        const out = []

        const p = root.parse(q, scoped)
        if (p) {
            out.push({
                key: "timer:new",
                kind: "timer",
                section: "Timers",
                icon: "hourglass_empty",
                title: "Start " + root.human(p.minutes) + " timer",
                subtitle: p.label ? "“" + p.label + "”" : "Ends at " + Qt.formatTime(new Date(Date.now() + p.minutes * 60000), "HH:mm"),
                badge: p.label ? "Ends " + Qt.formatTime(new Date(Date.now() + p.minutes * 60000), "HH:mm") : "",
                score: 1.55,
                pinned: true,
                activate: () => Timers.timer(p.minutes, p.label)
            })
        }

        if (/^(stop\s*watch|stopwatch)$/i.test(q) || (scoped && Fuzzy.score(q, "stopwatch") > 0.6)) {
            out.push({
                key: "timer:stopwatch",
                kind: "timer",
                section: "Timers",
                icon: "timer",
                title: "Start stopwatch",
                subtitle: "Counts up until you stop it",
                score: 1.2,
                activate: () => Timers.stopwatch()
            })
        }

        if (scoped || /^timers?$/i.test(q))
            for (const r of root.runningItems())
                out.push(r)

        if (scoped && !q) {
            [5, 10, 25].forEach((m, i) => out.push({
                key: "timer:preset:" + m,
                kind: "timer",
                section: "Quick start",
                icon: "hourglass_empty",
                title: m + " minutes",
                subtitle: "Ends at " + Qt.formatTime(new Date(Date.now() + m * 60000), "HH:mm"),
                score: 0.5 - i * 0.01,
                activate: () => Timers.timer(m, "")
            }))
            out.push({
                key: "timer:stopwatch",
                kind: "timer",
                section: "Quick start",
                icon: "timer",
                title: "Stopwatch",
                subtitle: "Counts up until you stop it",
                score: 0.46,
                activate: () => Timers.stopwatch()
            })
        }

        root.results = out
    }

    function clear() {
        root.last = ""
        root.results = []
    }

    readonly property Timer tick: Timer {
        interval: 1000
        repeat: true
        running: root.results.some(r => r.live)
        onTriggered: root.search(root.last, root.lastScoped)
    }
}
