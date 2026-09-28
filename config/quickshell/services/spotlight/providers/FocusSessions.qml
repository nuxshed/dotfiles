import QtQuick
import Quickshell
import Quickshell.Io
import Quickshell.Hyprland
import "../../../config"
import "../../../services"
import ".."

Provider {
    id: root

    name: "focus"
    label: "Focus"
    prefix: "fo"
    icon: "center_focus_strong"
    keywords: ["focus", "pomodoro", "deep work", "focus session"]
    weight: 1.05
    cap: 3

    entries: [
        { id: "start", title: "Start focus session", subtitle: "25 min · Do Not Disturb, hides distracting apps", icon: "center_focus_strong", keywords: ["focus", "start focus", "deep work", "pomodoro"], run: () => root.startFocus(0, true) },
        { id: "break", title: "Take a break", subtitle: "5 min · notifications back on", icon: "local_cafe", keywords: ["break", "take a break", "rest"], run: () => root.startBreak(0) }
    ]

    property bool managed: false
    property bool hideApps: true
    property var hidden: []
    property bool ready: false

    function classOf(t) {
        return (t.wayland?.appId || t.lastIpcObject?.class || "").toLowerCase()
    }

    function distractions() {
        return Hyprland.toplevels.values.filter(t => {
            const cls = root.classOf(t)
            return cls && SpotlightConfig.focusApps.some(a => cls.includes(a)) && !(t.workspace?.name ?? "").startsWith("special")
        })
    }

    function names(list) {
        const seen = []
        for (const t of list) {
            const n = DesktopEntries.heuristicLookup(root.classOf(t))?.name ?? root.classOf(t)
            if (!seen.includes(n))
                seen.push(n)
        }
        return seen.join(", ")
    }

    function hide() {
        const next = root.hidden.slice()
        for (const t of root.distractions()) {
            if (next.some(h => h.address === t.address))
                continue
            next.push({ address: t.address, workspace: t.workspace?.id ?? 1 })
            Hyprland.dispatch(`hl.dsp.window.move({ workspace = 'special:focus', window = 'address:0x${t.address}' })`)
        }
        root.hidden = next
        root.persist()
    }

    function restore() {
        for (const h of root.hidden)
            if (Hyprland.toplevels.values.some(t => t.address === h.address))
                Hyprland.dispatch(`hl.dsp.window.move({ workspace = ${h.workspace}, window = 'address:0x${h.address}' })`)
        root.hidden = []
        root.persist()
    }

    function apply(status) {
        if (!root.managed || !root.ready)
            return
        if (status === "focus") {
            Notifications.dnd = true
            if (root.hideApps)
                root.hide()
        } else if (status === "break" || status === "none") {
            Notifications.dnd = false
            root.restore()
            if (status === "none")
                root.managed = false
        }
        root.persist()
    }

    function startFocus(minutes, hide) {
        root.managed = true
        root.hideApps = hide
        if (!Pomodoro.active)
            Pomodoro.start()
        else if (Pomodoro.status === "break")
            Pomodoro.focus()
        else if (Pomodoro.status === "paused")
            Pomodoro.resume()
        if (minutes > 0)
            Pomodoro.extend(Math.round(minutes * 60 - Pomodoro.target))
        root.apply("focus")
    }

    function startBreak(minutes) {
        if (!Pomodoro.active)
            Pomodoro.start()
        Pomodoro.takeBreak()
        if (minutes > 0)
            Pomodoro.extend(Math.round(minutes * 60 - Pomodoro.target))
        root.managed = true
        root.apply("break")
    }

    function stop() {
        Pomodoro.end()
        root.managed = true
        root.apply("none")
    }

    function minutes(text) {
        if (!text)
            return 0
        const m = text.match(/^(\d+(?:\.\d+)?)\s*(h|hrs?|hours?|m|mins?|minutes?)?$/)
        if (!m)
            return -1
        return parseFloat(m[1]) * (m[2] && m[2][0] === "h" ? 60 : 1)
    }

    function search(text, scoped) {
        const q = text.trim().toLowerCase()
        const out = []
        if (!q && !scoped) {
            root.results = []
            return
        }

        const f = q.match(/^(?:start\s+)?(?:a\s+)?(?:focus|deep\s*work|pomodoro|work)(?:\s+(?:session|mode|block))?(?:\s+(?:for\s+)?(.+))?$/)
        const b = q.match(/^(?:take\s+)?(?:a\s+)?(?:(short|long)\s+)?break(?:\s+(?:for\s+)?(.+))?$/)
        const s = /^(?:end|stop|finish|quit)\s+(?:focus|pomodoro|session|deep\s*work)/.test(q)

        if (Pomodoro.active && (scoped || f || b || s || /^(focus|pomodoro)/.test(q))) {
            const actions = [
                Pomodoro.status === "break" ? { icon: "center_focus_strong", title: "Back to focus", run: () => root.startFocus(0, root.hideApps) } : { icon: "local_cafe", title: "Take a break", run: () => root.startBreak(0) },
                { icon: "add", title: "Add 5 minutes", run: () => Pomodoro.extend(300) },
                { icon: "stop", title: "End session", run: () => root.stop() }
            ]
            out.push({
                key: "focus:status",
                kind: "focus",
                section: "Focus",
                icon: Pomodoro.status === "break" ? "local_cafe" : "center_focus_strong",
                title: Pomodoro.label + " · " + Pomodoro.display + (Pomodoro.overtime ? " over" : " left"),
                subtitle: (Notifications.dnd ? "Do Not Disturb on" : "Notifications on") + (root.hidden.length ? " · " + root.hidden.length + " apps hidden" : ""),
                live: Pomodoro.status === "focus",
                score: s ? 1.2 : 1.5,
                pinned: true,
                primary: Pomodoro.status === "paused" ? "Resume" : Pomodoro.status === "focus" ? "Pause" : "Back to focus",
                activate: Pomodoro.status === "break" ? () => root.startFocus(0, root.hideApps) : () => Pomodoro.toggle(),
                altActivate: () => root.stop(),
                altHint: "End session",
                altIcon: "stop",
                actions: actions
            })
        }

        if (f || (scoped && !b)) {
            const min = f ? root.minutes(f[1] ?? "") : root.minutes(q)
            if (min >= 0) {
                const length = min || Pomodoro.targets.focus / 60
                const apps = root.distractions()
                out.push({
                    key: "focus:start",
                    kind: "focus",
                    section: "Focus",
                    icon: "center_focus_strong",
                    title: "Start focus · " + Math.round(length) + " min",
                    subtitle: "Do Not Disturb on" + (apps.length ? " · hides " + root.names(apps) : ""),
                    score: 1.55,
                    pinned: true,
                    activate: () => root.startFocus(length, true),
                    altActivate: () => root.startFocus(length, false),
                    altHint: "Start without hiding apps",
                    altIcon: "center_focus_strong"
                })
            }
        }

        if (b) {
            const min = root.minutes(b[2] ?? "")
            if (min >= 0) {
                const length = min || (b[1] === "long" ? Pomodoro.targets.long : Pomodoro.targets.short) / 60
                out.push({
                    key: "focus:break",
                    kind: "focus",
                    section: "Focus",
                    icon: "local_cafe",
                    title: "Take a " + Math.round(length) + " min break",
                    subtitle: "Notifications back on" + (root.hidden.length ? " · brings back hidden apps" : ""),
                    score: 1.55,
                    pinned: true,
                    activate: () => root.startBreak(length)
                })
            }
        }

        if (s && Pomodoro.active)
            out.push({
                key: "focus:stop",
                kind: "focus",
                section: "Focus",
                icon: "stop",
                title: "End focus session",
                subtitle: "Turns off Do Not Disturb and restores apps",
                score: 1.55,
                pinned: true,
                activate: () => root.stop()
            })

        root.results = out
    }

    function persist() {
        if (root.ready)
            state.setText(JSON.stringify({ managed: root.managed, hideApps: root.hideApps, hidden: root.hidden }))
    }

    readonly property Connections watcher: Connections {
        target: Pomodoro

        function onStatusChanged() {
            root.apply(Pomodoro.status)
        }
    }

    readonly property FileView state: FileView {
        path: SpotlightConfig.stateDir + "/focus.json"
        printErrors: false
        onLoaded: {
            try {
                const d = JSON.parse(text())
                root.managed = d.managed ?? false
                root.hideApps = d.hideApps ?? true
                root.hidden = d.hidden ?? []
            } catch (e) {}
            root.ready = true
            if (root.managed && Pomodoro.status === "focus")
                Notifications.dnd = true
            if (root.managed && Pomodoro.status !== "focus")
                root.apply(Pomodoro.status)
        }
        onLoadFailed: root.ready = true
    }
}
