import QtQuick
import Quickshell
import Quickshell.Io
import Quickshell.Hyprland
import "../../../config"
import "../../../services"
import ".."

Provider {
    id: root

    name: "clipboard"
    label: "Clipboard history"
    prefix: "v"
    icon: "content_paste"
    keywords: ["clipboard", "clipboard history", "paste", "copied"]
    mixed: false
    weight: 1

    readonly property string cache: Quickshell.cachePath("clipboard")
    readonly property var terminals: /wezterm|kitty|foot|alacritty|ghostty|konsole|terminal|xterm/i

    property string pending: ""
    property bool active: false
    property var lines: []
    property var images: ({})
    property var pins: []

    function kind(preview) {
        const bin = preview.match(/^\[\[ binary data (.+?) (\w+) (\d+x\d+) \]\]$/)
        if (bin)
            return { type: "image", size: bin[1], format: bin[2].toUpperCase(), dims: bin[3].replace("x", "×") }
        if (/^\[\[ binary data/.test(preview))
            return { type: "binary" }
        if (/^https?:\/\/\S+$/.test(preview.trim()))
            return { type: "link" }
        if (/^#([0-9a-f]{3}|[0-9a-f]{6}|[0-9a-f]{8})$/i.test(preview.trim()))
            return { type: "color" }
        return { type: "text" }
    }

    function pasteKeys() {
        const cls = Hyprland.activeToplevel?.wayland?.appId || Hyprland.activeToplevel?.lastIpcObject?.class || ""
        return root.terminals.test(cls) ? "wtype -M ctrl -M shift -k v -m shift -m ctrl" : "wtype -M ctrl -k v -m ctrl"
    }

    function decode(line, paste) {
        Quickshell.execDetached(["bash", "-c", `printf '%s' "$1" | cliphist decode | wl-copy${paste ? "; sleep 0.25; " + root.pasteKeys() : ""}`, "bash", line])
    }

    function copyText(text, paste) {
        Quickshell.execDetached(["bash", "-c", `printf '%s' "$1" | wl-copy${paste ? "; sleep 0.25; " + root.pasteKeys() : ""}`, "bash", text])
    }

    function remove(line) {
        Quickshell.execDetached(["bash", "-c", `printf '%s' "$1" | cliphist delete`, "bash", line])
    }

    function pin(text, on) {
        root.pins = on ? [text].concat(root.pins.filter(p => p !== text)) : root.pins.filter(p => p !== text)
        pinFile.setText(JSON.stringify(root.pins))
    }

    function search(text, scoped) {
        root.pending = text.trim()
        root.active = true
        if (!lister.running)
            lister.running = true
        root.build()
    }

    function clear() {
        root.active = false
        root.pending = ""
        root.results = []
    }

    function build() {
        const q = root.pending
        const out = []

        root.pins.forEach((p, i) => {
            const s = q ? Fuzzy.score(q, p) : 1.2 - i * 0.001
            if (s < 0)
                return
            out.push({
                key: "clip:pin:" + i,
                kind: "clipboard",
                section: "Pinned",
                icon: "bookmark",
                title: p.replace(/\s+/g, " ").slice(0, 200),
                subtitle: p.length + " characters",
                score: s + 0.3,
                pinned: true,
                primary: "Paste",
                activate: () => root.copyText(p, true),
                altActivate: () => root.copyText(p, false),
                altHint: "Copy",
                altIcon: "content_copy",
                actions: [{ icon: "close", title: "Unpin", run: () => root.pin(p, false) }]
            })
        })

        const decodeQueue = []
        root.lines.forEach((line, i) => {
            const tab = line.indexOf("\t")
            const id = line.slice(0, tab)
            const preview = line.slice(tab + 1)
            const k = root.kind(preview)
            const label = k.type === "image" ? "image " + k.format + " " + k.dims : preview
            const s = q ? Fuzzy.score(q, label) : 1 - i * 0.001
            if (s < 0 || out.length > 80)
                return

            const image = k.type === "image" ? root.images[id] ?? "" : ""
            if (k.type === "image" && !image && decodeQueue.length < 12)
                decodeQueue.push(line)

            const actions = []
            if (k.type === "link")
                actions.push({ icon: "open_in_new", title: "Open link", run: () => Quickshell.execDetached(["xdg-open", preview.trim()]) })
            if (k.type === "image" && image)
                actions.push({ icon: "image", title: "Open in Preview", run: () => Preview.openFile(image) })
            if (k.type === "text" || k.type === "link" || k.type === "color")
                actions.push({ icon: "bookmark", title: "Pin", run: () => root.pinDecoded(line) })
            actions.push({ icon: "delete", title: "Delete from history", run: () => root.remove(line) })
            actions.push({ icon: "delete_sweep", title: "Clear entire history (" + root.lines.length + " items, pins kept)", run: () => Quickshell.execDetached(["cliphist", "wipe"]) })

            out.push({
                key: "clip:" + id,
                kind: "clipboard",
                section: "History",
                icon: { image: "image", link: "link", color: "palette", binary: "archive" }[k.type] ?? "content_paste",
                art: image ? "file://" + image : "",
                dot: k.type === "color" ? preview.trim() : "",
                title: k.type === "image" ? "Image" : preview,
                subtitle: k.type === "image" ? k.format + " · " + k.dims + " · " + k.size : k.type === "link" ? "Link" : k.type === "color" ? "Colour" : preview.length >= 100 ? "Long text" : "Text",
                badge: i === 0 ? "Current" : "",
                score: s,
                pinned: true,
                primary: "Paste",
                activate: () => root.decode(line, true),
                altActivate: () => root.decode(line, false),
                altHint: "Copy",
                altIcon: "content_copy",
                actions: actions
            })
        })

        root.results = out
        if (decodeQueue.length && !decoder.running) {
            decoder.command = ["bash", "-c", 'd="$1"; shift; mkdir -p "$d"; for l; do id=${l%%$\'\\t\'*}; [ -s "$d/$id.img" ] || printf "%s" "$l" | cliphist decode > "$d/$id.img"; echo "$id"; done', "bash", root.cache].concat(decodeQueue)
            decoder.running = true
        }
    }

    function pinDecoded(line) {
        pinner.command = ["bash", "-c", `printf '%s' "$1" | cliphist decode`, "bash", line]
        pinner.running = true
    }

    readonly property Process lister: Process {
        command: ["cliphist", "list"]
        stdout: StdioCollector {
            onStreamFinished: {
                root.lines = text.split("\n").filter(l => l.indexOf("\t") > 0)
                if (root.active)
                    root.build()
            }
        }
    }

    readonly property Process decoder: Process {
        stdout: StdioCollector {
            onStreamFinished: {
                const next = Object.assign({}, root.images)
                for (const id of text.split("\n"))
                    if (id)
                        next[id] = root.cache + "/" + id + ".img"
                root.images = next
                if (root.active)
                    root.build()
            }
        }
    }

    readonly property Process pinner: Process {
        stdout: StdioCollector {
            onStreamFinished: {
                if (text.length > 0)
                    root.pin(text, true)
            }
        }
    }

    readonly property FileView pinFile: FileView {
        path: SpotlightConfig.stateDir + "/clipboard-pins.json"
        printErrors: false
        onLoaded: {
            try {
                root.pins = JSON.parse(text()) ?? []
            } catch (e) {
                root.pins = []
            }
        }
    }
}
