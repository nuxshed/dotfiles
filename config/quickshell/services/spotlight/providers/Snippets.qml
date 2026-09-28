import QtQuick
import Quickshell
import "../../../config"
import ".."

Provider {
    id: root

    name: "snippets"
    label: "Snippets"
    prefix: ";"
    icon: "subject"
    keywords: ["snippets", "text expansion", "templates"]
    weight: 0.9
    cap: 2

    function uuid() {
        return "xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx".replace(/[xy]/g, c => {
            const r = Math.random() * 16 | 0
            return (c === "x" ? r : (r & 0x3 | 0x8)).toString(16)
        })
    }

    function expand(text) {
        const now = new Date()
        return text
            .replace(/\{date\}/g, Qt.formatDate(now, "d MMMM yyyy"))
            .replace(/\{time\}/g, Qt.formatTime(now, "HH:mm"))
            .replace(/\{datetime\}/g, Qt.formatDateTime(now, "d MMM yyyy, HH:mm"))
            .replace(/\{iso\}/g, Qt.formatDateTime(now, Qt.ISODate))
            .replace(/\{uuid\}/g, root.uuid())
    }

    function deliver(text, type) {
        const out = type ? 'sleep 0.25; wtype -- "$t"' : 'printf %s "$t" | wl-copy'
        Quickshell.execDetached(["bash", "-c", 't="$1"; if [[ $t == *"{clipboard}"* ]]; then c=$(wl-paste -n 2>/dev/null); t="${t//\\{clipboard\\}/$c}"; fi; ' + out, "bash", text])
    }

    function search(text, scoped) {
        const q = text.trim().toLowerCase()
        const out = []
        if (!scoped && q.length < 3) {
            root.results = []
            return
        }

        for (const s of SpotlightConfig.snippets) {
            const exact = s.keyword === q
            const sc = !q ? 0.5 : exact ? 1.5 : scoped ? Fuzzy.best(q, [s.keyword, s.name]) : Fuzzy.score(q, s.name)
            if (sc < (scoped ? 0.3 : 0.8))
                continue
            const preview = root.expand(s.text).replace(/\{clipboard\}/g, "‹clipboard›").replace(/\n/g, " ↵ ")
            out.push({
                key: "snippet:" + s.keyword,
                kind: "snippets",
                section: "Snippets",
                icon: "subject",
                title: s.name,
                subtitle: preview,
                badge: ";" + s.keyword,
                score: sc,
                pinned: exact,
                primary: "Paste into window",
                activate: () => root.deliver(root.expand(s.text), true),
                altActivate: () => root.deliver(root.expand(s.text), false),
                altHint: "Copy to clipboard",
                altIcon: "content_copy"
            })
        }
        out.sort((a, b) => b.score - a.score)
        root.results = out
    }
}
