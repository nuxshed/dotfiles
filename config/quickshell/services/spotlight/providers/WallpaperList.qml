import QtQuick
import "../../../config"
import "../../../services"
import ".."

Provider {
    id: root

    name: "wallpapers"
    label: "Wallpapers"
    prefix: "wp"
    icon: "wallpaper"
    keywords: ["wallpaper", "wallpapers", "background", "desktop background"]
    weight: 1
    cap: 3
    entries: [
        { id: "shuffle", title: "Shuffle wallpaper", subtitle: "Pick a random wallpaper", icon: "shuffle", keywords: ["random wallpaper", "shuffle wallpaper", "next wallpaper"], run: () => Wallpapers.random() }
    ]

    function search(text, scoped) {
        const q = text.trim().toLowerCase().replace(/^(?:wallpapers?|backgrounds?)\s*/, "")
        const explicit = q !== text.trim().toLowerCase()
        if (!q && !scoped && !explicit) {
            root.results = []
            return
        }
        const out = []
        for (const w of Wallpapers.list) {
            const s = q ? Fuzzy.best(q, [w.name]) : 0.5
            if (s < (scoped || explicit ? 0.4 : 0.8))
                continue
            const current = Settings.wallpaper === w.path
            out.push({
                key: "wallpaper:" + w.path,
                kind: "wallpapers",
                section: "Wallpapers",
                art: "file://" + w.path,
                title: w.name,
                subtitle: current ? "Current wallpaper" : "Set as wallpaper",
                badge: current ? "Current" : "",
                score: q ? (explicit ? 1.2 + s * 0.1 : s) : explicit ? 0.95 : 0.5,
                activate: current ? null : () => Wallpapers.set(w.path)
            })
        }
        root.results = out
    }
}
