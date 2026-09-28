import QtQuick
import Quickshell
import "../../../services"
import ".."

Provider {
    id: root

    name: "notes"
    label: "Notes"
    prefix: "n"
    icon: "subject"
    keywords: ["notes", "note"]
    weight: 0.95
    cap: 3

    function snippet(body, q) {
        const flat = body.replace(/\s+/g, " ").trim()
        const i = q ? flat.toLowerCase().indexOf(q.toLowerCase()) : -1
        if (i < 0) {
            const lines = body.split("\n").map(l => l.trim()).filter(l => l.length > 0)
            return lines.slice(1).join(" · ").slice(0, 120)
        }
        const start = Math.max(0, i - 40)
        return (start > 0 ? "…" : "") + flat.slice(start, i + q.length + 80)
    }

    function open(id) {
        Notes.select(id)
        Notes.open = true
    }

    function create(text) {
        Notes.add()
        Notes.setBody(text)
    }

    function search(text, scoped) {
        const q = text.trim()
        const out = []

        if (q.length >= (scoped ? 0 : 2)) {
            for (const n of Notes.notes) {
                const body = n.body ?? ""
                if (!body.trim())
                    continue
                const title = Notes.title(n)
                const t = q ? Fuzzy.score(q, title) : 0.5
                const b = q && body.toLowerCase().includes(q.toLowerCase()) ? 0.7 : -1
                const s = Math.max(t, b)
                if (s < 0)
                    continue
                const lines = body.split("\n").filter(l => l.trim().length > 0).length
                out.push({
                    key: "note:" + n.id,
                    kind: "notes",
                    view: "note",
                    section: "Notes",
                    icon: "subject",
                    title: title,
                    subtitle: root.snippet(body, t >= b ? "" : q),
                    badge: lines + (lines === 1 ? " line" : " lines"),
                    score: s,
                    activate: () => root.open(n.id),
                    altActivate: () => Quickshell.execDetached(["wl-copy", "--", body]),
                    altHint: "Copy note",
                    altIcon: "content_copy"
                })
            }
        }

        if (scoped && q.length > 0)
            out.push({
                key: "note:new",
                kind: "notes",
                section: "Notes",
                icon: "add",
                title: "New note",
                subtitle: q,
                score: 0.3,
                pinned: true,
                activate: () => root.create(q)
            })

        root.results = out
    }
}
