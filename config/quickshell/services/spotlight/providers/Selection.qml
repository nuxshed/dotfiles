import QtQuick
import Quickshell
import Quickshell.Io
import Quickshell.Hyprland
import "../../../config"
import "../../../services"
import ".."

Provider {
    id: root

    name: "selection"
    label: "Selection"
    prefix: "sel"
    icon: "subject"
    keywords: ["selection", "selected text"]
    weight: 0.95
    cap: 2

    property string text: ""
    property string source: ""

    readonly property string trimmed: root.text.trim()
    readonly property int words: root.trimmed ? root.trimmed.split(/\s+/).length : 0
    readonly property int lineCount: root.trimmed ? root.trimmed.split("\n").length : 0
    readonly property string preview: root.trimmed.replace(/\s+/g, " ").slice(0, 120)
    readonly property string stats: root.words + (root.words === 1 ? " word · " : " words · ") + root.trimmed.length + " chars" + (root.lineCount > 1 ? " · " + root.lineCount + " lines" : "")

    readonly property var terminals: /wezterm|kitty|foot|alacritty|ghostty|konsole|terminal|xterm/i

    function capture() {
        root.text = ""
        root.source = ""
        grabber.running = false
        grabber.running = true
    }

    function pasteKeys() {
        const cls = Hyprland.activeToplevel?.wayland?.appId || Hyprland.activeToplevel?.lastIpcObject?.class || ""
        return root.terminals.test(cls) ? "wtype -M ctrl -M shift -k v -m shift -m ctrl" : "wtype -M ctrl -k v -m ctrl"
    }

    function put(value, replace) {
        Quickshell.execDetached(["bash", "-c", `printf '%s' "$1" | wl-copy${replace ? "; sleep 0.25; " + root.pasteKeys() : ""}`, "bash", value])
    }

    function open(url) {
        Quickshell.execDetached(["xdg-open", url])
    }

    function titleCase(s) {
        return s.toLowerCase().replace(/(^|[\s(\-"'])([a-z])/g, (m, a, b) => a + b.toUpperCase())
    }

    readonly property var actions: {
        const t = root.trimmed
        if (!t)
            return []
        const url = /^(https?:\/\/|www\.)\S+$/.test(t)
        const path = /^(~|\/)[^\n]*$/.test(t) && t.length < 400
        const letters = /[a-z]/i.test(t)
        const word = /^[a-z][a-z' -]*$/i.test(t) && root.words <= 3
        const math = /\d/.test(t) && /^[\d\s+\-*/^().,%x×÷]+$/.test(t) && /[+\-*/^x×÷%]/.test(t)
        let json = false
        if (/^[\[{]/.test(t)) {
            try {
                JSON.parse(t)
                json = true
            } catch (e) {}
        }
        const replace = root.source === "primary"
        const q = encodeURIComponent(t.slice(0, 1500))
        const list = []
        const add = (id, title, icon, keywords, extra) => list.push(Object.assign({ id: id, title: title, icon: icon, keywords: keywords }, extra))

        if (url)
            add("open", "Open link", "open_in_new", ["open", "link", "browser"], { run: () => root.open(t.startsWith("http") ? t : "https://" + t) })
        if (path)
            add("path", "Open path", "folder_open", ["open", "path", "file"], { run: () => root.open(t.replace(/^~/, SpotlightConfig.home)) })
        if (math)
            add("calc", "Calculate", "functions", ["calculate", "math", "evaluate"], { complete: "= " + t })
        if (word)
            add("define", "Define", "book", ["define", "meaning", "dictionary"], { complete: "d " + t })
        if (!url && !path)
            add("search", "Search the web", "search", ["search", "google", "web", "duckduckgo"], { run: () => root.open("https://duckduckgo.com/?q=" + q) })
        add("claude", "Ask Claude", "chat", ["ask", "claude", "ai", "explain"], { run: () => root.open("https://claude.ai/new?q=" + q) })
        if (letters && !url && !path)
            add("translate", "Translate", "language", ["translate", "english"], { run: () => root.open("https://translate.google.com/?sl=auto&tl=en&op=translate&text=" + q) })
        if (json)
            add("json", "Pretty-print JSON", "code", ["json", "pretty", "format"], { run: () => root.put(JSON.stringify(JSON.parse(t), null, 2), false) })
        if (root.words <= 5 && !url)
            add("files", "Find files", "folder", ["files", "find", "locate"], { complete: "f " + t })
        if (t.length <= 200 && root.lineCount === 1)
            add("task", "Add as task", "add", ["task", "todo", "remember"], { run: () => Tasks.add(t) })
        add("note", "New note", "subject", ["note", "save", "notes"], { run: () => { Notes.add(); Notes.setBody(t) } })
        if (letters && t.length <= 4000) {
            const verb = replace ? "Make " : "Copy as "
            add("upper", verb + "UPPERCASE", "keyboard_arrow_up", ["uppercase", "upper", "caps"], { run: () => root.put(t.toUpperCase(), replace) })
            add("lower", verb + "lowercase", "keyboard_arrow_down", ["lowercase", "lower"], { run: () => root.put(t.toLowerCase(), replace) })
            add("title", verb + "Title Case", "title", ["title case", "capitalize"], { run: () => root.put(root.titleCase(t), replace) })
        }
        if (replace)
            add("copy", "Copy", "content_copy", ["copy", "clipboard"], { run: () => root.put(t, false) })
        return list
    }

    function item(a, i, home) {
        return {
            key: "sel:" + a.id,
            kind: "selection",
            section: root.source === "primary" ? "Selection" : "Clipboard",
            icon: a.icon,
            title: home ? a.title : a.title + (root.source === "primary" ? " · selection" : " · clipboard"),
            subtitle: "“" + root.preview + "”",
            score: 0.9 - i * 0.001,
            pinned: true,
            activate: a.run ?? null,
            complete: a.complete ?? ""
        }
    }

    readonly property var homeItems: root.actions.slice(0, 8).map((a, i) => root.item(a, i, true))

    property string last: ""
    property bool lastScoped: false

    function clear() {
        root.last = ""
        root.results = []
    }

    function search(text, scoped) {
        root.last = text
        root.lastScoped = !!scoped
        const q = text.trim().toLowerCase()
        if (!root.trimmed || (!scoped && q.length < 3)) {
            root.results = []
            return
        }
        const out = []
        root.actions.forEach((a, i) => {
            const s = q ? Fuzzy.best(q, [a.title].concat(a.keywords)) : 0.9 - i * 0.001
            if (s < (scoped ? 0.3 : 0.8))
                return
            const it = root.item(a, i, false)
            it.score = s
            out.push(it)
        })
        root.results = out
    }

    readonly property Process grabber: Process {
        command: ["bash", "-c", 'p=$(timeout 0.5 wl-paste -p -n -t text 2>/dev/null); if [ -n "${p//[[:space:]]/}" ]; then printf "primary\\n%s" "$p"; else c=$(timeout 0.5 wl-paste -n -t text 2>/dev/null); [ -n "${c//[[:space:]]/}" ] && printf "clipboard\\n%s" "$c"; fi']
        stdout: StdioCollector {
            onStreamFinished: {
                const nl = text.indexOf("\n")
                if (nl < 0)
                    return
                root.source = text.slice(0, nl)
                root.text = text.slice(nl + 1, nl + 1 + 10000)
                if (root.last)
                    root.search(root.last, root.lastScoped)
            }
        }
    }
}
