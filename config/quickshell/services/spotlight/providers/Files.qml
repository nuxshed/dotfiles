import QtQuick
import Quickshell
import Quickshell.Io
import "../../../config"
import "../../../services"
import ".."

Provider {
    id: root

    name: "files"
    label: "Files"
    prefix: "f"
    icon: "folder"
    keywords: ["files", "find file", "documents", "folders"]
    weight: 0.82
    cap: SpotlightConfig.maxFilesMixed

    property string pending: ""
    property var scored: ({})

    function basename(path) {
        const p = path.replace(/\/$/, "")
        const i = p.lastIndexOf("/")
        return i < 0 ? p : p.substring(i + 1)
    }

    function rawDirname(path) {
        const p = path.replace(/\/$/, "")
        const i = p.lastIndexOf("/")
        return i < 0 ? "/" : p.substring(0, i)
    }

    function pretty(path) {
        return path.startsWith(SpotlightConfig.home) ? "~" + path.substring(SpotlightConfig.home.length) : path
    }

    function suffix(path) {
        const b = root.basename(path)
        const i = b.lastIndexOf(".")
        return i > 0 ? b.substring(i + 1) : ""
    }

    function iconFor(path, isDir) {
        if (isDir)
            return "folder"
        if (/\.(png|jpe?g|gif|webp|svg|bmp|avif|heic)$/i.test(path))
            return "image"
        if (/\.(mp4|mkv|webm|mov|avi)$/i.test(path))
            return "movie"
        if (/\.(mp3|flac|wav|ogg|m4a|opus)$/i.test(path))
            return "music_note"
        if (/\.(pdf|epub)$/i.test(path))
            return "picture_as_pdf"
        if (/\.(zip|tar|gz|xz|zst|7z|rar)$/i.test(path))
            return "archive"
        if (/\.(qml|js|ts|tsx|jsx|py|rs|go|c|cpp|h|java|kt|nix|lua|sh|json|toml|ya?ml|xml|html|css)$/i.test(path))
            return "code"
        return "description"
    }

    readonly property var junk: /\/(node_modules|site-packages|\.git|\.cache|\.cargo|\.rustup|\.npm|\.gradle|\.m2|\.local\/share\/(uv|Steam|flatpak)|Android\/Sdk|go\/pkg|\.emacs\.d\/straight)\//

    function words(q) {
        return q.toLowerCase().split(/[\s_\-.]+/).filter(w => w.length > 0)
    }

    function norm(t) {
        return t.toLowerCase().replace(/[_\-.]+/g, " ")
    }

    function scorePath(q, path) {
        const base = root.basename(path)
        const nb = root.norm(base)
        const np = root.norm(path)
        const whole = Math.max(Fuzzy.score(q, base), Fuzzy.score(root.norm(q), nb))

        const ws = root.words(q)
        let sum = 0
        let ok = ws.length > 0
        for (const w of ws) {
            const b = Fuzzy.score(w, nb)
            if (b >= 0) {
                sum += b
                continue
            }
            const d = Fuzzy.score(w, np)
            if (d < 0) {
                ok = false
                break
            }
            sum += d * 0.6
        }

        let s = Math.max(whole, ok ? sum / ws.length : -1, Fuzzy.score(q, path) * 0.6)
        if (s < 0)
            return s

        const rel = path.startsWith(SpotlightConfig.home) ? path.substring(SpotlightConfig.home.length) : path
        if (root.junk.test(path))
            s -= 0.3
        else if (/\/\./.test(rel))
            s -= 0.15
        s -= Math.min(0.1, Math.max(0, rel.split("/").length - 4) * 0.02)
        return s
    }

    function size(n) {
        const u = ["B", "KB", "MB", "GB", "TB"]
        let i = 0
        while (n >= 1024 && i < u.length - 1) {
            n /= 1024
            i++
        }
        return (i === 0 ? n : n.toFixed(n < 10 ? 1 : 0)) + " " + u[i]
    }

    function ago(sec) {
        const d = Date.now() / 1000 - sec
        if (d < 60)
            return "just now"
        if (d < 3600)
            return Math.floor(d / 60) + "m ago"
        if (d < 86400)
            return Math.floor(d / 3600) + "h ago"
        if (d < 86400 * 30)
            return Math.floor(d / 86400) + "d ago"
        return Qt.formatDate(new Date(sec * 1000), d < 86400 * 365 ? "d MMM" : "MMM yyyy")
    }

    property string browseDir: ""
    property string browseFilter: ""
    property string listedDir: ""
    property var listing: []

    function normalize(path) {
        const out = []
        for (const part of path.split("/")) {
            if (!part || part === ".")
                continue
            if (part === "..")
                out.pop()
            else
                out.push(part)
        }
        return "/" + out.join("/")
    }

    function browse(q) {
        debounce.stop()
        const expanded = q.replace(/^~(?=\/|$)/, SpotlightConfig.home)
        const slash = expanded.lastIndexOf("/")
        const filter = expanded.slice(slash + 1)
        const dir = root.normalize(expanded.slice(0, slash) || "/")
        root.pending = ""
        root.browseDir = dir
        root.browseFilter = filter === ".." || filter === "." ? "" : filter
        if (filter === "..") {
            root.browseDir = root.normalize(dir + "/..")
        }
        if (root.listedDir === root.browseDir) {
            root.buildBrowse()
            return
        }
        lister.running = false
        lister.command = ["find", "-L", root.browseDir, "-mindepth", "1", "-maxdepth", "1", "-printf", "%y\t%s\t%T@\t%f\n"]
        lister.running = true
    }

    function clear() {
        debounce.stop()
        root.pending = ""
        root.browseDir = ""
        root.results = []
    }

    function acceptListing(text) {
        if (!root.browseDir)
            return
        const out = []
        for (const line of text.split("\n")) {
            const f = line.split("\t")
            if (f.length < 4)
                continue
            out.push({ dir: f[0] === "d", size: parseInt(f[1]), mtime: Math.floor(parseFloat(f[2])), name: f.slice(3).join("\t") })
        }
        out.sort((a, b) => (b.dir - a.dir) || a.name.localeCompare(b.name, undefined, { sensitivity: "base" }))
        root.listing = out
        root.listedDir = root.browseDir
        root.buildBrowse()
    }

    function buildBrowse() {
        const dir = root.browseDir
        const filter = root.browseFilter
        const base = dir === "/" ? "" : dir
        const shown = root.pretty(dir)
        const out = []

        if (!filter) {
            out.push({
                key: "browse:open:" + dir,
                kind: "files",
                section: shown,
                icon: "folder_open",
                title: "Open " + (dir === SpotlightConfig.home ? "home folder" : root.basename(dir) || "/"),
                subtitle: shown + " · " + root.listing.length + " items",
                score: 2,
                pinned: true,
                activate: () => Quickshell.execDetached(["xdg-open", dir]),
                altActivate: () => Quickshell.execDetached({ command: [SpotlightConfig.terminal], workingDirectory: dir }),
                altHint: "Open terminal here",
                altIcon: "code"
            })
            if (dir !== "/")
                out.push({
                    key: "browse:up",
                    kind: "files",
                    section: shown,
                    icon: "subdirectory_arrow_right",
                    title: "..",
                    subtitle: root.pretty(root.normalize(dir + "/..")),
                    score: 1.9,
                    pinned: true,
                    complete: (root.pretty(root.normalize(dir + "/..")) + "/").replace(/^\/\/$/, "/")
                })
        }

        let i = 0
        for (const e of root.listing) {
            if (e.name.startsWith(".") && !filter.startsWith("."))
                continue
            const sc = filter ? Fuzzy.score(filter, e.name) : 1.5 - i * 0.0001
            if (sc < 0)
                continue
            i++
            const path = base + "/" + e.name
            const it = root.itemFor(path, e.size, e.mtime, e.dir)
            it.section = shown
            it.score = sc
            it.pinned = true
            it.complete = root.pretty(path) + (e.dir ? "/" : "")
            if (e.dir) {
                it.activate = null
                it.primary = "Open folder"
                it.altActivate = () => Quickshell.execDetached(["xdg-open", path])
                it.altHint = "Open in file manager"
            }
            out.push(it)
        }
        out.sort((a, b) => b.score - a.score)
        root.results = out.slice(0, 80)
    }

    function search(text) {
        const q = text.trim()
        if (/^(~\/|~$|\/)/.test(q))
            return root.browse(q)
        root.listedDir = ""
        root.browseDir = ""
        if (q.length < 2) {
            root.pending = ""
            root.results = []
            debounce.stop()
            return
        }

        root.pending = q
        debounce.restart()
    }

    function run() {
        proc.running = false
        proc.command = ["plocate", "-d", SpotlightConfig.indexDb, "-i", "-l", "1500", "--"].concat(root.pending.split(/\s+/).filter(w => w.length > 0))
        proc.running = true
    }

    function accept(output) {
        if (!root.pending)
            return
        const scored = {}
        const paths = []

        for (const path of output.split("\n")) {
            if (!path)
                continue
            const s = root.scorePath(root.pending, path)
            if (s >= 0) {
                scored[path] = s
                paths.push(path)
            }
        }

        paths.sort((a, b) => scored[b] - scored[a])
        root.scored = scored

        if (paths.length === 0) {
            root.results = []
            return
        }

        stat.running = false
        stat.command = ["stat", "-c", "%s\t%Y\t%F\t%n", "--"].concat(paths.slice(0, SpotlightConfig.maxScoped))
        stat.running = true
    }

    function itemFor(path, bytes, mtime, isDir) {
        const dir = root.rawDirname(path)
        const ext = root.suffix(path)
        const thumbable = !isDir && Thumbs.kindFor({ isDir: false, suffix: ext }) !== ""
        const image = /^(png|jpe?g|gif|webp|bmp|avif|heic|svg)$/i.test(ext)
        const actions = []

        if (image)
            actions.push({ icon: "image", title: "Open in Preview", run: () => Preview.openFile(path) })
        actions.push({ icon: "code", title: "Open terminal here", run: () => Quickshell.execDetached({ command: [SpotlightConfig.terminal], workingDirectory: isDir ? path : dir }) })
        if (!isDir)
            actions.push({ icon: "content_paste", title: "Copy file", run: () => Quickshell.execDetached(["wl-copy", "-t", "text/uri-list", "file://" + encodeURI(path)]) })

        return {
            key: "file:" + path,
            kind: "files",
            view: "file",
            section: "Files",
            icon: root.iconFor(path, isDir),
            thumb: thumbable ? { path: path, mtime: String(mtime), size: bytes, isDir: false, suffix: ext } : null,
            title: root.basename(path),
            subtitle: root.pretty(dir),
            badge: isDir ? "Folder" : root.size(bytes) + " · " + root.ago(mtime),
            score: (root.scored[path] ?? 0) + 0.08 * Math.pow(0.5, Math.max(0, Date.now() / 1000 - mtime) / (14 * 86400)),
            copy: path,
            copyTitle: "Copy path",
            activate: () => Quickshell.execDetached(["xdg-open", path]),
            altActivate: () => Quickshell.execDetached([SpotlightConfig.fileManager, isDir ? path : dir]),
            altHint: "Reveal in file manager",
            altIcon: "folder_open",
            actions: actions,
            details: [
                { label: "Location", value: root.pretty(dir) },
                { label: "Modified", value: Qt.formatDateTime(new Date(mtime * 1000), "ddd d MMM yyyy, HH:mm") },
                { label: isDir ? "Kind" : "Size", value: isDir ? "Folder" : root.size(bytes) + " (" + bytes.toLocaleString(Qt.locale("en_US"), "f", 0) + " bytes)" }
            ]
        }
    }

    function acceptStat(output) {
        if (!root.pending)
            return
        const out = []
        for (const line of output.split("\n")) {
            const f = line.split("\t")
            if (f.length < 4)
                continue
            const path = f.slice(3).join("\t")
            out.push(root.itemFor(path, parseInt(f[0]), parseInt(f[1]), f[2] === "directory"))
        }
        out.sort((a, b) => b.score - a.score)
        root.results = out
    }

    readonly property Timer debounce: Timer {
        interval: SpotlightConfig.fileDebounce
        onTriggered: root.run()
    }

    readonly property Process proc: Process {
        stdout: StdioCollector {
            onStreamFinished: root.accept(text)
        }
        onExited: (code) => {
            if (code !== 0)
                root.results = []
        }
    }

    readonly property Process lister: Process {
        stdout: StdioCollector {
            onStreamFinished: root.acceptListing(text)
        }
    }

    readonly property Process stat: Process {
        stdout: StdioCollector {
            onStreamFinished: root.acceptStat(text)
        }
    }
}
