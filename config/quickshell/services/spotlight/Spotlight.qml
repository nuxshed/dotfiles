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
    property bool expanded: false
    property int action: 0

    signal queryReset

    readonly property list<QtObject> providers: [apps, calc, countdowns, zones, dictionary, toggles, windowActions, media, events, todos, quicklinks, snippets, keybinds, selection, focusMode, notes, web, files, windows, commands, units, nixpkgs, manpages, vault, clipboard, procs]

    readonly property bool home: root.query.length === 0

    function prefixed(q) {
        const m = q.match(/^([a-z>]+)\s+([\s\S]*)$/)
        if (!m)
            return null
        for (const p of root.providers)
            if (p.prefix === m[1])
                return { provider: p, term: m[2] }
        return null
    }

    function scopeFor(q) {
        if (q.startsWith("!"))
            return web
        if (q.startsWith("="))
            return calc
        if (q.startsWith("~") || q.startsWith("/"))
            return files
        if (q.startsWith(";"))
            return snippets
        const p = root.prefixed(q)
        if (p)
            return p.provider
        if (q.startsWith(">"))
            return commands
        return null
    }

    function termFor(q) {
        if (q.startsWith("!") || q.startsWith("=") || q.startsWith("~") || q.startsWith("/"))
            return q
        if (q.startsWith(";"))
            return q.substring(1)
        const p = root.prefixed(q)
        if (p)
            return p.term
        if (q.startsWith(">"))
            return q.substring(1).trim()
        return q
    }

    readonly property var scope: root.scopeFor(root.query)

    function itemFor(key) {
        if (key.startsWith("app:"))
            return apps.itemForId(key.substring(4))
        if (key.startsWith("cmd:"))
            return commands.itemForId(key.substring(4))
        return null
    }

    readonly property var suggestions: {
        DesktopEntries.applications.values
        const out = []
        const seen = {}
        const add = it => {
            if (it && !seen[it.key] && out.length < SpotlightConfig.homeApps) {
                seen[it.key] = true
                out.push(it)
            }
        }
        for (const k of Frecency.ranked())
            add(root.itemFor(k))
        for (const id of SpotlightConfig.defaultApps)
            add(apps.itemForId(id))
        return out
    }

    property var results: []
    property var launcherItems: []

    function scheduleMerge() {
        if (!mergeTimer.running)
            mergeTimer.start()
    }

    readonly property Timer mergeTimer: Timer {
        interval: 0
        onTriggered: root.merge()
    }

    function merge() {
        if (root.home) {
            root.results = root.suggestions.concat(selection.homeItems)
            return
        }

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

        if (!scoped)
            for (const l of root.launcherItems)
                merged.push({ item: l, rank: l.score * (l.pinned ? 1 : Frecency.boost(l.key)) })

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

        if (scoped || merged.some(e => !e.item.fallback && e.rank >= 0.6))
            root.results = out
        else
            root.results = out.filter(i => !i.fallback).concat(root.fallbacks(root.query.trim()))
    }

    function launchers(q) {
        const lower = q.toLowerCase()
        const out = []
        if (lower.length < 2)
            return out
        for (const p of root.providers) {
            if (p.prefix && p.label) {
                const exact = lower === p.prefix && p.prefix.length >= 2
                const s = exact ? 3 : Fuzzy.best(lower, [p.label].concat(p.keywords))
                if (s >= 0.62)
                    out.push({
                        key: "scope:" + p.name,
                        kind: "scope",
                        section: exact ? "Search in" : "Go to",
                        icon: p.icon || "search",
                        title: p.label,
                        subtitle: "Type “" + p.prefix + " ” to search " + p.label.toLowerCase(),
                        badge: p.prefix,
                        score: exact ? 3 : s * 0.93,
                        pinned: exact,
                        primary: "Open " + p.label.toLowerCase(),
                        complete: p.prefix + " "
                    })
            }
            for (const e of p.entries) {
                const s = Fuzzy.best(lower, [e.title].concat(e.keywords ?? []))
                if (s < 0.62)
                    continue
                out.push({
                    key: "entry:" + p.name + ":" + e.id,
                    kind: "entry",
                    section: p.label,
                    icon: e.icon ?? p.icon,
                    title: e.title,
                    subtitle: e.subtitle ?? "",
                    score: s,
                    activate: e.run ?? null,
                    complete: e.complete ?? ""
                })
            }
        }
        return out
    }

    function fallbacks(t) {
        if (t.length < 2)
            return []
        const section = "Use “" + (t.length > 24 ? t.slice(0, 24) + "…" : t) + "” with"
        const search = (id, bang, title) => ({
            key: "fb:" + id, kind: "fallback", section: section, icon: "search", favicon: SpotlightConfig.bangs[bang].domain,
            title: title, subtitle: SpotlightConfig.bangs[bang].domain, score: 0,
            activate: () => web.open(web.searchUrl(bang, t))
        })
        const scope = (id, prefix, icon, title, subtitle) => ({
            key: "fb:" + id, kind: "fallback", section: section, icon: icon, title: title, subtitle: subtitle, badge: prefix.trim(), score: 0,
            complete: prefix + t
        })
        const out = [
            search("ddg", "ddg", "Search DuckDuckGo"),
            search("google", "g", "Search Google"),
            { key: "fb:claude", kind: "fallback", section: section, icon: "chat", favicon: "claude.ai", title: "Ask Claude", subtitle: "claude.ai", score: 0,
              activate: () => web.open("https://claude.ai/new?q=" + encodeURIComponent(t)) },
            scope("files", "f ", "folder", "Search files", "Everything in your home folder"),
            scope("nix", "nix ", "widgets", "Search nixpkgs", "nixpkgs-unstable packages")
        ]
        if (/^[a-z][a-z' -]*$/i.test(t) && t.split(" ").length <= 3)
            out.push(scope("define", "d ", "book", "Define", "Wiktionary"))
        return out
    }

    readonly property var current: root.results[root.selected] ?? null

    function actionsFor(c) {
        if (!c)
            return []
        const out = []
        if (c.activate)
            out.push({ icon: "keyboard_return", title: c.primary ?? "Open", hint: "↵", run: c.activate })
        if (c.altActivate)
            out.push({ icon: c.altIcon ?? "open_in_new", title: c.altHint ?? "Alternate action", hint: "⌥↵", run: c.altActivate })
        for (const a of c.actions ?? [])
            out.push(a)
        if (c.copy && !out.some(a => a.copies))
            out.push({ icon: "content_copy", title: c.copyTitle ?? "Copy", copies: true, run: () => Quickshell.execDetached(["wl-copy", "--", c.copy]) })
        return out
    }

    readonly property var currentActions: root.actionsFor(root.current)

    onQueryChanged: {
        root.selected = 0
        root.expanded = false
        root.dispatch()
        root.scheduleMerge()
    }

    onSuggestionsChanged: root.scheduleMerge()

    Component.onCompleted: {
        for (const p of root.providers)
            p.resultsChanged.connect(root.scheduleMerge)
        selection.homeItemsChanged.connect(root.scheduleMerge)
        Frecency.entriesChanged.connect(root.scheduleMerge)
    }

    onSelectedChanged: root.expanded = false

    function dispatch() {
        const q = root.query
        const scoped = root.scopeFor(q)
        const t = root.termFor(q)
        root.launcherItems = scoped || q.length === 0 ? [] : root.launchers(q.trim())

        for (const p of root.providers) {
            if (q.length === 0)
                p.clear()
            else if (scoped) {
                if (p === scoped)
                    p.search(t, true)
                else
                    p.clear()
            } else if (p.mixed) {
                p.search(t, false)
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
        if (root.expanded) {
            const a = root.currentActions.length
            if (a > 0)
                root.action = (root.action + delta + a) % a
            return
        }
        const n = root.results.length
        if (n === 0)
            return
        root.selected = (root.selected + delta + n) % n
    }

    function vertical(delta) {
        if (!root.home || root.expanded)
            return root.move(delta)
        const n = root.suggestions.length
        if (delta > 0 && root.selected < n && selection.homeItems.length > 0)
            root.selected = n
        else if (delta < 0 && root.selected >= n)
            root.selected = 0
    }

    function toggleActions() {
        if (root.expanded) {
            root.expanded = false
            return
        }
        const c = root.current
        if (!c || (root.currentActions.length < 2 && !(c.details?.length > 0)))
            return
        root.action = 0
        root.expanded = true
    }

    function complete() {
        const c = root.current
        if (c && c.complete)
            root.setQuery(c.complete)
    }

    function run(c, fn) {
        if (!c.pinned)
            Frecency.record(c.key)
        root.hide()
        Qt.callLater(fn)
    }

    function activate(alt) {
        const c = root.current
        if (!c)
            return

        if (root.expanded) {
            const a = root.currentActions[root.action]
            if (a)
                root.run(c, a.run)
            return
        }

        const fn = alt ? (c.altActivate ?? c.activate) : c.activate
        if (!fn) {
            if (c.complete)
                root.setQuery(c.complete)
            return
        }
        root.run(c, fn)
    }

    function back() {
        if (root.expanded)
            root.expanded = false
        else
            root.hide()
    }

    function show() {
        selection.capture()
        root.setQuery("")
        root.selected = 0
        root.expanded = false
        root.open = true
    }

    function hide() {
        root.open = false
        root.expanded = false
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
    readonly property Countdowns countdowns: Countdowns {}
    readonly property Zones zones: Zones {}
    readonly property Dictionary dictionary: Dictionary {}
    readonly property NoteSearch notes: NoteSearch {}
    readonly property Units units: Units {}
    readonly property Nixpkgs nixpkgs: Nixpkgs {}
    readonly property Manpages manpages: Manpages {}
    readonly property Toggles toggles: Toggles {}
    readonly property WindowActions windowActions: WindowActions {}
    readonly property MediaControls media: MediaControls {}
    readonly property Events events: Events {}
    readonly property Todos todos: Todos {}
    readonly property Quicklinks quicklinks: Quicklinks {}
    readonly property Snippets snippets: Snippets {}
    readonly property Vault vault: Vault {}
    readonly property Keybinds keybinds: Keybinds {}
    readonly property Selection selection: Selection {}
    readonly property FocusSessions focusMode: FocusSessions {}
}
