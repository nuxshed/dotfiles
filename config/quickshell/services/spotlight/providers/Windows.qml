import QtQuick
import Quickshell
import Quickshell.Hyprland
import ".."

Provider {
    id: root

    name: "windows"
    label: "Windows"
    prefix: "w"
    icon: "web_asset"
    keywords: ["windows", "switch window", "open windows"]
    weight: 1.05

    function appClass(t) {
        return t.wayland?.appId || t.lastIpcObject?.class || ""
    }

    function focus(t) {
        Hyprland.dispatch(`hl.dsp.focus({ window = 'address:0x${t.address}' })`)
    }

    function search(text) {
        const out = []

        for (const t of Hyprland.toplevels.values) {
            if (!t.title && !root.appClass(t))
                continue

            const cls = root.appClass(t)
            const s = Fuzzy.best(text, [t.title, cls])
            if (s < 0)
                continue

            const entry = cls ? DesktopEntries.heuristicLookup(cls) : null

            out.push({
                key: "win:" + t.address,
                kind: "windows",
                section: "Open windows",
                icon: entry?.icon ?? "web_asset",
                iconIsImage: !!entry?.icon,
                fallbackIcon: "web_asset",
                title: t.title || cls,
                subtitle: cls + (t.workspace ? " · workspace " + t.workspace.name : ""),
                score: s * 0.98,
                activate: () => root.focus(t),
                altHint: "Close window",
                altActivate: () => Hyprland.dispatch(`hl.dsp.window.close({ window = 'address:0x${t.address}' })`)
            })
        }

        root.results = out
    }
}
