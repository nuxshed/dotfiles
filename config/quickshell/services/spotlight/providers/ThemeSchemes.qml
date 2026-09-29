import QtQuick
import "../../../config"
import "../../../services"
import ".."

Provider {
    id: root

    name: "themes"
    label: "Themes"
    prefix: "th"
    icon: "palette"
    keywords: ["theme", "themes", "colours", "colors", "colour scheme", "color scheme", "appearance", "dark mode"]
    weight: 1.05
    cap: 4
    entries: [
        { id: "picker", title: "Theme picker", subtitle: "Browse themes with a live preview", icon: "palette", keywords: ["theme picker", "change theme", "switch theme"], run: () => Pickers.show("theme") },
        { id: "wallpapers", title: "Wallpaper picker", subtitle: "Choose a wallpaper", icon: "wallpaper", keywords: ["wallpaper picker", "change wallpaper", "background"], run: () => Pickers.show("wallpaper") },
        { id: "settings", title: "Settings", subtitle: "Appearance, desktop, sound, network and more", icon: "settings", keywords: ["settings", "preferences", "configuration", "control panel"], run: () => SettingsApp.show("") }
    ]

    function search(text, scoped) {
        const q = text.trim().toLowerCase().replace(/^(?:colou?r\s*)?(?:themes?|schemes?)\s*/, "")
        const explicit = q !== text.trim().toLowerCase()
        if (!q && !scoped && !explicit) {
            root.results = []
            return
        }
        const out = []
        for (const t of Themes.list) {
            const s = q ? Fuzzy.best(q, [t.name, t.id]) : 0.5
            if (s < (scoped || explicit ? 0.4 : 0.75))
                continue
            const current = Settings.theme === t.id
            out.push({
                key: "theme:" + t.id,
                kind: "themes",
                section: "Themes",
                theme: t.id,
                title: t.name,
                subtitle: t.dynamic ? "Accent taken from your wallpaper" : current ? "Current theme" : "Apply theme",
                badge: current ? "Current" : "",
                score: q ? (explicit ? 1.2 + s * 0.1 : s) : explicit ? 0.95 : 0.5,
                activate: current ? null : () => Settings.set("theme", t.id)
            })
        }
        root.results = out
    }
}
