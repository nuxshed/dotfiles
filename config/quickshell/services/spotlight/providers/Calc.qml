import QtQuick
import Quickshell
import Quickshell.Io
import "../../../config"
import ".."

Provider {
    id: root

    name: "calc"
    label: "Calculator"
    prefix: "="
    weight: 1

    property string pending: ""

    readonly property var byteUnits: ["kb", "mb", "gb", "tb", "pb", "kib", "mib", "gib", "tib"]
    readonly property var operators: /[+\-*/^%]|\bto\b|\bin\b|\bas\b/
    readonly property var incomplete: /[+\-*/^%(,.]\s*$/

    function normalise(text) {
        let q = text
        q = q.replace(/\bin\b/gi, "to")
        q = q.replace(/\b(\d*\.?\d*)\s*(kib|mib|gib|tib|kb|mb|gb|tb|pb)\b/gi, (m, n, u) => {
            const up = u.length === 3 && u.charAt(1).toLowerCase() === "i"
                ? u.charAt(0).toUpperCase() + "iB"
                : u.charAt(0).toUpperCase() + "B"
            return n + " " + up
        })
        return q
    }

    function looksNumeric(text) {
        if (!/\d/.test(text))
            return false
        if (root.incomplete.test(text))
            return false
        return root.operators.test(text) || /^\s*[\d.]+\s*[a-z%]+\s*$/i.test(text)
    }

    function search(text) {
        let q = text.trim()
        const explicit = q.startsWith("=")
        if (explicit)
            q = q.substring(1).trim()

        if (!q || (!explicit && !root.looksNumeric(q))) {
            root.pending = ""
            root.results = []
            proc.running = false
            return
        }

        root.pending = q
        proc.running = false
        proc.command = ["qalc", "-t", "-m", "500", root.normalise(q)]
        proc.running = true
    }

    function accept(output) {
        const value = output.trim()
        if (!value || value === root.pending || /^error/i.test(value)) {
            root.results = []
            return
        }

        root.results = [{
            key: "calc",
            kind: "calc",
            section: "Calculator",
            icon: "calculate",
            title: value,
            subtitle: root.pending,
            score: 1.6,
            pinned: true,
            copy: value,
            altHint: "Copy result",
            activate: () => Quickshell.execDetached(["wl-copy", "--", value]),
            altActivate: () => Quickshell.execDetached(["wl-copy", "--", value])
        }]
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
