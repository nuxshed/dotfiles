import QtQuick
import Quickshell
import Quickshell.Hyprland
import ".."

Provider {
    id: root

    name: "window"
    label: "Window"
    prefix: "win"
    icon: "web_asset"
    keywords: ["window", "window commands", "workspace"]
    weight: 1
    cap: 3

    function run(t, call) {
        for (const c of [].concat(call))
            Hyprland.dispatch(c.replace("$W", `window = 'address:0x${t.address}'`))
    }

    readonly property var commands: [
        { id: "float", title: "Toggle floating", icon: "open_with", keywords: ["float", "floating", "tile", "unfloat"], call: "hl.dsp.window.float({ $W })" },
        { id: "fullscreen", title: "Toggle fullscreen", icon: "fullscreen", keywords: ["fullscreen", "full screen"], call: "hl.dsp.window.fullscreen({ $W })" },
        { id: "maximize", title: "Toggle maximize", icon: "crop_free", keywords: ["maximize", "maximise", "max", "zoom"], call: "hl.dsp.window.fullscreen({ $W, mode = 'maximized' })" },
        { id: "center", title: "Center window", icon: "filter_center_focus", keywords: ["center", "centre", "middle"], call: "hl.dsp.window.center({ $W })" },
        { id: "pin", title: "Toggle pin on all workspaces", icon: "picture_in_picture_alt", keywords: ["pin", "sticky", "always visible"], call: "hl.dsp.window.pin({ $W })" },
        { id: "fit", title: "Fit column to screen", icon: "view_column", keywords: ["fit", "column", "scroll"], call: "hl.dsp.layout('fit active')" },
        { id: "scratch", title: "Send to scratchpad", icon: "archive", keywords: ["scratchpad", "hide", "stash", "special"], call: "hl.dsp.window.move({ workspace = 'special:magic', $W })" },
        { id: "close", title: "Close window", icon: "close", keywords: ["close", "quit window"], call: "hl.dsp.window.close({ $W })" },
        { id: "kill", title: "Force quit window", icon: "cancel", keywords: ["kill", "force quit", "force close"], call: "hl.dsp.window.kill({ $W })" }
    ]

    function describe(t) {
        const cls = t.wayland?.appId || t.lastIpcObject?.class || ""
        return (t.title || cls) + (t.workspace ? " · workspace " + t.workspace.name : "")
    }

    function item(t, id, title, icon, score, call) {
        const e = DesktopEntries.heuristicLookup(t.wayland?.appId || t.lastIpcObject?.class || "")
        return {
            key: "win:" + id,
            kind: "window",
            section: "Window",
            icon: icon,
            title: title,
            subtitle: root.describe(t),
            badge: e?.name ?? "",
            score: score,
            activate: () => root.run(t, call)
        }
    }

    function search(text, scoped) {
        const t = Hyprland.activeToplevel
        const q = text.trim().toLowerCase()
        if (!t || (!q && !scoped)) {
            root.results = []
            return
        }

        const out = []
        const move = q.match(/^(?:move|send|throw)(?:\s+(?:window|it))?\s+to\s+(?:workspace\s+|ws\s+)?(\d{1,2})$/)
        if (move) {
            const n = parseInt(move[1])
            out.push(root.item(t, "move", "Move window to workspace " + n, "open_with", 1.55, `hl.dsp.window.move({ workspace = ${n}, $W })`))
            out.push(root.item(t, "follow", "Move window to workspace " + n + " and follow", "open_with", 1.54, [`hl.dsp.window.move({ workspace = ${n}, $W })`, `hl.dsp.focus({ workspace = ${n} })`]))
        }

        const go = q.match(/^(?:go\s+to\s+|switch\s+to\s+)?(?:workspace|ws)\s+(\d{1,2})$/)
        if (go) {
            const n = parseInt(go[1])
            out.push({
                key: "win:goto",
                kind: "window",
                section: "Workspace",
                icon: "view_carousel",
                title: "Go to workspace " + n,
                subtitle: (Hyprland.workspaces.values.find(w => w.id === n)?.toplevels?.values?.length ?? 0) + " windows",
                score: 1.55,
                pinned: true,
                activate: () => Hyprland.dispatch(`hl.dsp.focus({ workspace = ${n} })`)
            })
        }

        for (const c of root.commands) {
            const s = q ? Fuzzy.best(q.replace(/\s*(the\s+)?(window|this)$/, ""), [c.title].concat(c.keywords)) : 0.5
            if (s < (scoped ? 0.4 : 0.75))
                continue
            out.push(root.item(t, c.id, c.title, c.icon, s, c.call))
        }

        root.results = out
    }
}
