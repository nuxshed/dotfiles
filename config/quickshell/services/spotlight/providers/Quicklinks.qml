import QtQuick
import Quickshell
import "../../../config"
import ".."

Provider {
    id: root

    name: "quicklinks"
    label: "Quicklinks"
    prefix: "ql"
    icon: "link"
    keywords: ["links", "bookmarks", "quicklinks"]
    weight: 1
    cap: 3

    function fill(link, arg) {
        return link.url.replace("%s", encodeURIComponent(arg).replace(/%2F/gi, "/"))
    }

    function item(link, arg, score, pinned) {
        const needs = link.url.includes("%s")
        const ready = !needs || arg.length > 0
        const url = ready ? root.fill(link, arg) : ""
        return {
            key: "ql:" + link.keyword,
            kind: "quicklinks",
            section: "Quicklinks",
            icon: link.icon ?? "link",
            favicon: link.domain ?? "",
            title: needs && arg ? link.name + ": " + arg : link.name,
            subtitle: ready ? url : "Type " + (link.hint ?? "a value") + " after “" + link.keyword + "”",
            badge: link.keyword,
            score: score,
            pinned: pinned,
            complete: link.keyword + " ",
            activate: ready ? () => Quickshell.execDetached(["xdg-open", url]) : null,
            copy: url,
            copyTitle: "Copy URL"
        }
    }

    function search(text, scoped) {
        const q = text.trim()
        const lower = q.toLowerCase()
        const out = []
        if (!q && !scoped) {
            root.results = []
            return
        }

        const space = lower.indexOf(" ")
        const head = space < 0 ? lower : lower.slice(0, space)
        const arg = space < 0 ? "" : q.slice(space + 1).trim()

        for (const l of SpotlightConfig.quicklinks) {
            if (head === l.keyword && (arg || !l.url.includes("%s") || space < 0)) {
                out.push(root.item(l, arg, arg ? 1.45 : 1.3, true))
                continue
            }
            if (arg && !scoped)
                continue
            const s = !q ? 0.5 : Fuzzy.best(lower, [l.name, l.keyword])
            if (s < (scoped ? 0.3 : 0.8))
                continue
            out.push(root.item(l, "", s, false))
        }
        out.sort((a, b) => b.score - a.score)
        root.results = out
    }
}
