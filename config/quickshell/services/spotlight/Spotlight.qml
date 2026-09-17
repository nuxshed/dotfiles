pragma Singleton
pragma ComponentBehavior: Bound

import QtQuick
import Quickshell
import "../../config"
import "providers"

Singleton {
    id: root

    property bool open: false
    property string query: ""
    property int selected: 0

    signal queryReset

    readonly property list<QtObject> providers: [apps, calc, web, files, windows, commands, clipboard, procs]

    function scopeFor(q) {
        if (q.startsWith("!"))
            return web
        if (q.startsWith("="))
            return calc
        if (q.startsWith("~") || q.startsWith("/"))
            return files

        const m = q.match(/^([a-z>])\s+/)
        if (m) {
            for (const p of root.providers)
                if (p.prefix === m[1])
                    return p
        }
        if (q.startsWith(">"))
            return commands
        return null
    }

    function termFor(q) {
        if (q.startsWith("!") || q.startsWith("="))
            return q
        if (q.startsWith("~") || q.startsWith("/"))
            return q
        if (root.scopeFor(q)) {
            const m = q.match(/^[a-z>]\s+([\s\S]*)$/)
            if (m)
                return m[1]
            return q.substring(1).trim()
        }
        return q
    }

    readonly property var scope: root.scopeFor(root.query)
    readonly property string term: root.termFor(root.query)

    readonly property var results: {
        const merged = []
        const scoped = root.scopeFor(root.query)
        const limit = scoped ? SpotlightConfig.maxScoped : SpotlightConfig.maxMixed

        for (const p of root.providers) {
            const rs = p.results
            if (scoped) {
                if (p !== scoped)
                    continue
            } else if (!p.mixed) {
                continue
            }

            let taken = 0
            for (const r of rs) {
                if (!scoped && p.cap > 0 && taken >= p.cap)
                    break
                taken++
                merged.push({
                    item: r,
                    rank: r.score * p.weight * (r.pinned ? 1 : Frecency.boost(r.key))
                })
            }
        }

        merged.sort((a, b) => b.rank - a.rank)

        const top = merged.slice(0, limit)
        const order = []
        const groups = ({})

        for (const e of top) {
            const sec = e.item.section ?? ""
            if (!groups[sec]) {
                groups[sec] = []
                order.push(sec)
            }
            groups[sec].push(e.item)
        }

        const out = []
        for (const sec of order)
            for (const item of groups[sec])
                out.push(item)
        return out
    }

    readonly property var current: root.results[root.selected] ?? null

    onQueryChanged: {
        root.selected = 0
        root.dispatch()
    }

    function dispatch() {
        const q = root.query
        const scoped = root.scopeFor(q)
        const t = root.termFor(q)

        for (const p of root.providers) {
            if (scoped) {
                if (p === scoped)
                    p.search(t)
                else
                    p.clear()
            } else if (p.mixed) {
                p.search(t)
            } else {
                p.clear()
            }
        }
    }

    function setQuery(text) {
        root.query = text
        root.queryReset()
    }

    function move(delta) {
        const n = root.results.length
        if (n === 0)
            return
        root.selected = (root.selected + delta + n) % n
    }

    function complete() {
        const c = root.current
        if (c && c.complete)
            root.setQuery(c.complete)
    }

    function activate(alt) {
        const c = root.current
        if (!c)
            return

        const fn = alt ? (c.altActivate ?? c.activate) : c.activate
        if (!fn) {
            if (c.complete)
                root.setQuery(c.complete)
            return
        }

        if (!c.pinned)
            Frecency.record(c.key)

        root.hide()
        Qt.callLater(fn)
    }

    function show() {
        root.setQuery("")
        root.selected = 0
        root.open = true
    }

    function hide() {
        root.open = false
    }

    function toggle() {
        if (root.open)
            root.hide()
        else
            root.show()
    }

    readonly property Apps apps: Apps {}
    readonly property Calc calc: Calc {}
    readonly property Web web: Web {}
    readonly property Files files: Files {}
    readonly property Windows windows: Windows {}
    readonly property Commands commands: Commands {}
    readonly property Clipboard clipboard: Clipboard {}
    readonly property Procs procs: Procs {}
}
