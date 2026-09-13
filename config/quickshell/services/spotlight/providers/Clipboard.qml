import QtQuick
import Quickshell
import Quickshell.Io
import ".."

Provider {
    id: root

    name: "clipboard"
    label: "Clipboard"
    prefix: "v"
    mixed: false
    weight: 1

    property string pending: ""
    property bool available: true

    function search(text) {
        root.pending = text.trim()
        proc.running = false
        proc.running = true
    }

    function decode(id) {
        copyProc.running = false
        copyProc.command = ["bash", "-c", "cliphist decode " + id + " | wl-copy"]
        copyProc.running = true
    }

    function accept(output) {
        const lines = output.split("\n").filter(l => l.length > 0)
        const out = []

        for (const line of lines) {
            const tab = line.indexOf("\t")
            if (tab < 0)
                continue

            const id = line.substring(0, tab)
            const preview = line.substring(tab + 1)
            const s = root.pending ? Fuzzy.score(root.pending, preview) : 0.5
            if (s < 0)
                continue

            out.push({
                key: "clip:" + id,
                kind: "clipboard",
                section: "Clipboard",
                icon: /^\[\[ binary/.test(preview) ? "image" : "content_paste",
                title: preview,
                subtitle: "Copy to clipboard",
                score: root.pending ? s : 0.5 - out.length * 0.001,
                activate: () => root.decode(id)
            })
        }

        root.results = out.slice(0, 60)
    }

    readonly property Process proc: Process {
        command: ["cliphist", "list"]
        stdout: StdioCollector {
            onStreamFinished: root.accept(text)
        }
        onExited: (code) => {
            root.available = code === 0
            if (code !== 0)
                root.results = []
        }
    }

    readonly property Process copyProc: Process {}
}
