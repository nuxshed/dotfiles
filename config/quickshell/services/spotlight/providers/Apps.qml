import QtQuick
import Quickshell
import "../../../config"
import ".."

Provider {
    id: root

    name: "apps"
    label: "Applications"
    prefix: "a"
    weight: 1.15

    readonly property var entries: DesktopEntries.applications.values
    property string lastQuery: ""
    property bool primed: false

    onEntriesChanged: if (root.primed) root.search(root.lastQuery)

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

    function search(text) {
        root.lastQuery = text
        root.primed = true

        const out = []

        for (const e of root.entries) {
            if (e.noDisplay)
                continue

            const lower = text.toLowerCase()
            const rest = lower.replace(e.name.toLowerCase(), "").trim()
            const named = rest !== lower

            const s = Fuzzy.best(text, root.entryTargets(e))
            if (s < 0 && !named)
                continue

            if (s >= 0)
                out.push({
                key: "app:" + e.id,
                kind: "apps",
                section: "Applications",
                icon: e.icon,
                iconIsImage: true,
                fallbackIcon: "apps",
                title: e.name,
                subtitle: e.genericName || e.comment || "",
                score: s,
                activate: () => root.launch(e),
                altActivate: () => root.launchInTerminal(e),
                altHint: "Run in terminal"
            })

            for (const a of e.actions) {
                if (!rest || !named)
                    continue

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
