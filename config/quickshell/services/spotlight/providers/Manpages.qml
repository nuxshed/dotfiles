import QtQuick
import Quickshell
import Quickshell.Io
import "../../../config"
import ".."

Provider {
    id: root

    name: "man"
    label: "Man pages"
    prefix: "man"
    icon: "description"
    keywords: ["manual", "man pages", "man page", "docs"]
    mixed: false
    weight: 1

    property var pages: []
    property var descriptions: ({})
    property string pending: ""
    property bool scanned: false

    function parse(text) {
        const seen = {}
        const out = []
        for (const path of text.split("\n")) {
            if (!path)
                continue
            const file = path.slice(path.lastIndexOf("/") + 1).replace(/\.(gz|bz2|xz|zst|lzma)$/, "")
            const dot = file.lastIndexOf(".")
            if (dot <= 0)
                continue
            const page = { name: file.slice(0, dot), section: file.slice(dot + 1), path: path }
            const id = page.name + "." + page.section
            if (seen[id])
                continue
            seen[id] = true
            out.push(page)
        }
        root.pages = out
        root.scanned = true
        root.search(root.pending)
    }

    function clean(text) {
        return text.replace(/\\f[BIRP]|\\f\(..|\\&|\\\//g, "").replace(/\\-/g, "-").replace(/\\\(em|\\\(en/g, "—").replace(/\\(.)/g, "$1").replace(/^.*?\s+[-—]+\s+/, "").trim()
    }

    function describe(list) {
        const missing = list.filter(p => !(p.path in root.descriptions)).map(p => p.path)
        if (missing.length === 0)
            return
        reader.running = false
        reader.command = ["sh", "-c", `
for f; do
  d=$(case "$f" in
    *.gz) zcat "$f" ;; *.bz2) bzcat "$f" ;; *.xz|*.lzma) xzcat "$f" ;; *.zst) zstdcat "$f" ;; *) cat "$f" ;;
  esac 2>/dev/null | awk '/^\\.S[Hh][ \\t]+"?NAME/{n=1;next} n&&/^\\.Nd/{sub(/^\\.Nd[ \\t]*/,"");print;exit} n&&/^\\.S[Hh]/{print b;exit} n&&!/^\\./{b=b" "$0} END{if(n&&b)print b}' | head -n 1)
  printf '%s\\t%s\\n' "$f" "$d"
done`, "sh"].concat(missing)
        reader.running = true
    }

    function acceptDescriptions(text) {
        const next = Object.assign({}, root.descriptions)
        for (const line of text.split("\n")) {
            const tab = line.indexOf("\t")
            if (tab > 0)
                next[line.slice(0, tab)] = root.clean(line.slice(tab + 1))
        }
        root.descriptions = next
        root.search(root.pending)
    }

    function clear() {
        root.pending = ""
        root.results = []
    }

    function itemFor(p, score) {
        const ref = p.section + " " + p.name
        const pdf = "/tmp/qs-man-" + p.name.replace(/[^\w.-]/g, "_") + "." + p.section + ".pdf"
        return {
            key: "man:" + p.name + "." + p.section,
            kind: "man",
            section: "Man pages",
            icon: "description",
            title: p.name,
            subtitle: root.descriptions[p.path] || "",
            badge: "(" + p.section + ")",
            score: score,
            primary: "Open in terminal",
            activate: () => Quickshell.execDetached([SpotlightConfig.terminal, "-e", "man", p.section, p.name]),
            altActivate: () => Quickshell.execDetached(["sh", "-c", 'man -Tpdf "$1" "$2" > "$3" && exec zathura "$3"', "sh", p.section, p.name, pdf]),
            altHint: "Open as PDF",
            altIcon: "picture_as_pdf",
            copy: "man " + ref,
            copyTitle: "Copy command",
            actions: [{ icon: "open_in_new", title: "Open on man.archlinux.org", run: () => Quickshell.execDetached(["xdg-open", "https://man.archlinux.org/man/" + encodeURIComponent(p.name) + "." + encodeURIComponent(p.section)]) }],
            details: [{ label: "File", value: p.path }]
        }
    }

    function search(text) {
        root.pending = text.trim()
        if (!root.scanned) {
            if (!scanner.running)
                scanner.running = true
            root.results = []
            return
        }
        const q = root.pending.toLowerCase()
        const first = q.match(/^([0-9n][a-z]*)\s+(\S+)$/)
        const last = q.match(/^(\S+?)\s*\(([0-9n][a-z]*)\)$|^(\S+)\s+([0-9n][a-z]*)$/)
        const name = first ? first[2] : last ? (last[1] ?? last[3]) : q
        const section = first ? first[1] : last ? (last[2] ?? last[4]) : ""
        if (!name) {
            root.results = []
            return
        }

        const scored = []
        for (const p of root.pages) {
            if (section && !p.section.startsWith(section))
                continue
            let s = Fuzzy.score(name, p.name)
            if (s < 0.5)
                continue
            if (p.name.toLowerCase() === name)
                s += 0.3 - (p.section === (section || "1") ? 0 : 0.05)
            scored.push({ p: p, s: s })
        }
        scored.sort((a, b) => b.s - a.s || a.p.name.length - b.p.name.length)
        const top = scored.slice(0, 30)
        root.results = top.map(e => root.itemFor(e.p, e.s))
        root.describe(top.slice(0, 14).map(e => e.p))
    }

    readonly property Process scanner: Process {
        command: ["sh", "-c", "for d in $(manpath 2>/dev/null | tr : ' '); do find -L \"$d\" -path '*/man[0-9n]*/*' -type f 2>/dev/null; done"]
        stdout: StdioCollector {
            onStreamFinished: root.parse(text)
        }
    }

    readonly property Process reader: Process {
        stdout: StdioCollector {
            onStreamFinished: root.acceptDescriptions(text)
        }
    }
}
