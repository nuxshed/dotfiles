import QtQuick
import Quickshell
import Quickshell.Io
import "../../../config"
import ".."

Provider {
    id: root

    name: "files"
    label: "Files"
    prefix: "f"
    weight: 0.82
    cap: SpotlightConfig.maxFilesMixed

    property string pending: ""

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

    function dirname(path) {
        const d = root.rawDirname(path)
        return d.startsWith(SpotlightConfig.home) ? "~" + d.substring(SpotlightConfig.home.length) : d
    }

    function iconFor(path) {
        if (path.endsWith("/"))
            return "folder"
        if (/\.(png|jpe?g|gif|webp|svg|bmp)$/i.test(path))
            return "image"
        if (/\.(mp4|mkv|webm|mov|avi)$/i.test(path))
            return "movie"
        if (/\.(mp3|flac|wav|ogg|m4a)$/i.test(path))
            return "music_note"
        if (/\.(pdf|epub)$/i.test(path))
            return "picture_as_pdf"
        if (/\.(zip|tar|gz|xz|zst|7z|rar)$/i.test(path))
            return "folder_zip"
        return "description"
    }

    function search(text) {
        const q = text.trim()
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
        proc.command = ["plocate", "-d", SpotlightConfig.indexDb, "-i", "-l", "200", "--", root.pending]
        proc.running = true
    }

    function accept(output) {
        const lines = output.split("\n").filter(l => l.length > 0)
        const out = []

        for (const path of lines) {
            const base = root.basename(path)
            const s = Math.max(Fuzzy.score(root.pending, base), Fuzzy.score(root.pending, path) * 0.8)
            if (s < 0)
                continue

            out.push({
                key: "file:" + path,
                kind: "files",
                section: "Files",
                icon: root.iconFor(path),
                title: base,
                subtitle: root.dirname(path),
                score: s,
                copy: path,
                altHint: "Reveal in file manager",
                activate: () => Quickshell.execDetached(["xdg-open", path]),
                altActivate: () => Quickshell.execDetached([SpotlightConfig.fileManager, root.rawDirname(path)])
            })
        }

        out.sort((a, b) => b.score - a.score)
        root.results = out.slice(0, SpotlightConfig.maxScoped)
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
}
