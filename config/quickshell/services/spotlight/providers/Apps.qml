import QtQuick
import Quickshell
import Quickshell.Hyprland
import "../../../config"
import ".."

Provider {
    id: root

    name: "apps"
    label: "Applications"
    prefix: "a"
    icon: "apps"
    keywords: ["apps", "applications", "programs", "launch"]
    weight: 1.15

    readonly property var desktop: DesktopEntries.applications.values
    property string lastQuery: ""
    property bool primed: false

    readonly property var index: root.desktop.filter(e => !e.noDisplay).map(e => ({ entry: e, targets: root.entryTargets(e), name: e.name.toLowerCase() }))

    onIndexChanged: if (root.primed) root.search(root.lastQuery)

    function entryTargets(e) {
        const t = [e.name]
        if (e.genericName)
            t.push(e.genericName)
        for (const k of e.keywords)
            t.push(k)
        if (e.id)
            t.push(e.id.replace(/\.desktop$/, ""))
        return t
    }

    function launch(entry) {
        Quickshell.execDetached({
            command: entry.command,
            workingDirectory: entry.workingDirectory
        })
    }

    function launchInTerminal(entry) {
        Quickshell.execDetached({
            command: [SpotlightConfig.terminal, "-e"].concat(entry.command),
            workingDirectory: entry.workingDirectory
        })
    }

    function focus(t) {
        Hyprland.dispatch(`hl.dsp.focus({ window = 'address:0x${t.address}' })`)
    }

    function windowMap() {
        const map = {}
        for (const t of Hyprland.toplevels.values) {
            const cls = t.wayland?.appId || t.lastIpcObject?.class || ""
            const e = cls ? DesktopEntries.heuristicLookup(cls) : null
            if (e)
                (map[e.id] ?? (map[e.id] = [])).push(t)
        }
        return map
    }

    function itemFor(e, score, wins) {
        const n = wins.length
        const actions = []
        for (const w of wins.slice(0, 4))
            actions.push({ icon: "open_with", title: "Switch to " + (w.title || e.name), run: () => root.focus(w) })
        for (const a of e.actions)
            actions.push({ icon: "subdirectory_arrow_right", title: a.name, run: () => a.execute() })

        return {
            key: "app:" + e.id,
            kind: "apps",
            view: "app",
            section: "Applications",
            icon: e.icon,
            iconIsImage: true,
            fallbackIcon: "apps",
            title: e.name,
            subtitle: e.genericName || e.comment || "",
            badge: n === 0 ? "" : n === 1 ? "Running" : n + " windows",
            live: n > 0,
            score: score,
            primary: n > 0 ? "Open new window" : "Open",
            activate: () => root.launch(e),
            altActivate: () => root.launchInTerminal(e),
            altHint: "Run in terminal",
            altIcon: "code",
            actions: actions,
            details: [
                { label: "Command", value: e.command.join(" ") },
                { label: "Desktop ID", value: e.id }
            ]
        }
    }

    function itemForId(id) {
        const e = DesktopEntries.byId(id)
        if (!e || e.noDisplay)
            return null
        return root.itemFor(e, 0.5, root.windowMap()[e.id] ?? [])
    }

    function search(text) {
        root.lastQuery = text
        root.primed = true

        const out = []
        const wins = root.windowMap()
        const lower = text.toLowerCase()

        for (const x of root.index) {
            const e = x.entry
            const rest = lower.replace(x.name, "").trim()
            const named = rest !== lower

            const s = Fuzzy.best(text, x.targets)
            if (s < 0 && !named)
                continue

            if (s >= 0)
                out.push(root.itemFor(e, s, wins[e.id] ?? []))

            if (!rest || !named)
                continue

            for (const a of e.actions) {
                const as = Fuzzy.score(rest, a.name)
                if (as < 0.4)
                    continue

                out.push({
                    key: "app:" + e.id + ":" + a.name,
                    kind: "apps",
                    section: "Applications",
                    icon: e.icon,
                    iconIsImage: true,
                    fallbackIcon: "apps",
                    title: e.name + " — " + a.name,
                    subtitle: "Action",
                    score: as * 0.92,
                    activate: () => a.execute()
                })
            }
        }

        root.results = out
    }
}
