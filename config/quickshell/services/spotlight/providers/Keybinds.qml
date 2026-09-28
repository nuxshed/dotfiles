import QtQuick
import Quickshell
import Quickshell.Io
import Quickshell.Hyprland
import "../../../config"
import ".."

Provider {
    id: root

    name: "keybinds"
    label: "Keybinds"
    prefix: "kb"
    icon: "keyboard"
    keywords: ["shortcuts", "hotkeys", "keybinds", "keys", "cheatsheet", "bindings"]
    weight: 1
    cap: 3

    property var binds: []

    readonly property var keyNames: ({
        super: "Super", shift: "Shift", ctrl: "Ctrl", control: "Ctrl", alt: "Alt", return: "Enter", space: "Space",
        period: ".", comma: ",", slash: "/", escape: "Esc", tab: "Tab", up: "↑", down: "↓", left: "←", right: "→",
        "mouse:272": "LMB", "mouse:273": "RMB", xf86audioraisevolume: "Vol +", xf86audiolowervolume: "Vol −",
        xf86monbrightnessup: "Bright +", xf86monbrightnessdown: "Bright −", xf86kbdbrightnessup: "Kbd +", xf86kbdbrightnessdown: "Kbd −"
    })

    readonly property var directions: ({ l: "left", r: "right", u: "up", d: "down" })

    function split(args) {
        const out = []
        let depth = 0
        let quote = ""
        let cur = ""
        for (const ch of args) {
            if (quote) {
                if (ch === quote)
                    quote = ""
            } else if (ch === "\"" || ch === "'") {
                quote = ch
            } else if ("({[".includes(ch)) {
                depth++
            } else if (")}]".includes(ch)) {
                depth--
            } else if (ch === "," && depth === 0) {
                out.push(cur.trim())
                cur = ""
                continue
            }
            cur += ch
        }
        if (cur.trim())
            out.push(cur.trim())
        return out
    }

    function evalKey(expr, vars, loop) {
        return expr.split("..").map(p => {
            const t = p.trim()
            const lit = t.match(/^["'](.*)["']$/)
            if (lit)
                return lit[1]
            if (loop && t === loop.name)
                return loop.label
            return vars[t] ?? t
        }).join("")
    }

    function keys(combo) {
        return combo.split("+").map(k => k.trim()).filter(k => k).map(k => {
            const n = root.keyNames[k.toLowerCase()]
            if (n)
                return n
            if (/^xf86/i.test(k))
                return k.replace(/^xf86/i, "")
            return k.length === 1 ? k.toUpperCase() : k
        })
    }

    function describe(dsp, loop) {
        const lit = s => (s.match(/["']([^"']*)["']/) ?? [])[1] ?? ""
        const field = (s, f) => (s.match(new RegExp(f + "\\s*=\\s*(?:[\"']([^\"']*)[\"']|([\\w.-]+))")) ?? [])
        const val = (s, f) => { const m = field(s, f); return m[1] ?? m[2] ?? "" }
        const sub = v => loop && v === loop.name ? loop.label : v

        let m
        if ((m = dsp.match(/^hl\.dsp\.exec_cmd\((.*)\)$/))) {
            const cmd = lit(m[1])
            const ipc = cmd.match(/^qs ipc call (\S+)\s+(.*)$/)
            if (ipc)
                return ipc[1].charAt(0).toUpperCase() + ipc[1].slice(1) + ": " + ipc[2]
            return cmd.includes(" ") ? "Run " + cmd : "Launch " + cmd
        }
        if (/^hl\.dsp\.focus\(/.test(dsp)) {
            if (val(dsp, "workspace"))
                return "Go to workspace " + sub(val(dsp, "workspace"))
            if (val(dsp, "direction"))
                return "Focus " + (root.directions[val(dsp, "direction")] ?? val(dsp, "direction"))
        }
        if (/^hl\.dsp\.window\.move\(/.test(dsp)) {
            if (val(dsp, "workspace"))
                return val(dsp, "workspace").startsWith("special") ? "Send window to scratchpad" : "Move window to workspace " + sub(val(dsp, "workspace"))
            if (val(dsp, "direction"))
                return "Move window " + (root.directions[val(dsp, "direction")] ?? val(dsp, "direction"))
        }
        if (/^hl\.dsp\.window\.resize\(/.test(dsp)) {
            const x = parseInt(val(dsp, "x") || "0")
            const y = parseInt(val(dsp, "y") || "0")
            return "Resize window " + (x ? (x > 0 ? "wider" : "narrower") : y > 0 ? "taller" : "shorter")
        }
        if ((m = dsp.match(/^hl\.dsp\.layout\((.*)\)$/)))
            return "Layout: " + lit(m[1])
        if ((m = dsp.match(/^hl\.dsp\.workspace\.toggle_special\((.*)\)$/)))
            return "Toggle scratchpad (" + lit(m[1]) + ")"
        const named = ({
            "window.close": "Close window", "window.float": "Toggle floating", "window.fullscreen": "Toggle fullscreen",
            "window.drag": "Drag window", "window.pin": "Pin window", "window.center": "Center window", "window.kill": "Force quit window",
            "group.toggle": "Toggle group", "group.next": "Next window in group", "exit": "Exit Hyprland"
        })
        const path = (dsp.match(/^hl\.dsp\.([\w.]+)\(/) ?? [])[1] ?? dsp
        return named[path] ?? path.replace(/[._]/g, " ")
    }

    function parse(text) {
        const vars = {}
        const out = []
        let group = "General"
        let loop = null

        for (const raw of text.split("\n")) {
            const line = raw.trim()
            let m
            if ((m = line.match(/^local\s+(\w+)\s*=\s*["']([^"']*)["']/)))
                vars[m[1]] = m[2]
            if ((m = line.match(/^--\s*([^-].*)$/)) && !/^-+$/.test(m[1]))
                group = m[1].replace(/\s*\(.*\)$/, "").replace(/:.*$/, "").trim()
            if ((m = line.match(/^for\s+(\w+)\s*=\s*(\d+)\s*,\s*(\d+)\s+do/)))
                loop = { name: m[1], label: m[2] + "–" + m[3] }
            else if (loop && /^end\b/.test(line))
                loop = null
            if (!(m = line.match(/^hl\.bind\((.*)\)\s*(--\s*(.*))?$/)))
                continue

            const args = root.split(m[1])
            if (args.length < 2)
                continue
            const combo = root.evalKey(args[0], vars, loop)
            const dsp = args[1]
            const mouse = /mouse\s*=\s*true/.test(args[2] ?? "")
            const click = /click\s*=\s*true/.test(args[2] ?? "")
            let desc = m[3] ? m[3].trim() : root.describe(dsp, loop)
            if (mouse)
                desc = (click ? "Click: " : "Hold: ") + desc.toLowerCase()
            const keys = root.keys(combo)
            out.push({ keys: keys, combo: combo, desc: desc, dsp: dsp, group: group, runnable: !loop && !mouse, lower: (desc + " " + keys.join(" ") + " " + group).toLowerCase() })
        }
        root.binds = out
    }

    function search(text, scoped) {
        const q0 = text.trim().toLowerCase()
        const keyword = /\b(shortcuts?|keybinds?|hotkeys?|bindings?|binds?|keys?)\b/
        const asked = keyword.test(q0)
        if (!scoped && q0.length < 3) {
            root.results = []
            return
        }
        const q = q0.replace(keyword, "").replace(/\b(for|to|what'?s|what is|the)\b/g, "").replace(/\s+/g, " ").trim()

        const out = []
        const loose = scoped || asked
        root.binds.forEach((b, i) => {
            if (!loose && !b.lower.includes(q))
                return
            const s = q ? Math.max(Fuzzy.score(q, b.desc), Fuzzy.score(q, b.keys.join(" ")) * 0.95, Fuzzy.score(q, b.group) * 0.7) : 0.5 - i * 0.001
            if (s < (scoped || asked ? (q ? 0.45 : 0) : 0.8))
                return
            out.push({
                key: "kb:" + b.combo + ":" + i,
                kind: "keybinds",
                section: scoped && !q ? b.group : "Keybinds",
                icon: "keyboard",
                title: b.desc,
                subtitle: b.group,
                keys: b.keys,
                score: scoped ? s : asked ? s + 0.5 : s * 0.97,
                pinned: true,
                primary: b.runnable ? "Run now" : "",
                activate: b.runnable ? () => Hyprland.dispatch(b.dsp) : null,
                copy: b.keys.join(" + "),
                copyTitle: "Copy shortcut"
            })
        })
        root.results = q ? out.sort((a, b) => b.score - a.score).slice(0, 40) : out
    }

    readonly property FileView file: FileView {
        path: SpotlightConfig.home + "/.config/hypr/config.lua"
        printErrors: false
        onLoaded: root.parse(text())
    }
}
