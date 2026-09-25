pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io
import "../config"

Singleton {
    id: root

    readonly property var palette: [Colors.blue, Colors.cyan, Colors.green, Colors.yellow, Colors.orange, Colors.red, Colors.magenta, Colors.textMuted, "#8fa8d6", "#7fc4c0", "#b5c97a", "#d9b98c", "#e88f72", "#e0a3a8", "#c9a0dc", "#6e7480"]

    property bool open: false
    property string view: "month"
    property date cursor: new Date()
    property date selected: new Date()
    property var editing: null
    property string query: ""

    property var local: []
    property var hidden: []
    property var colors: ({})
    property var names: ({})
    property var order: []
    property int counter: 0
    property bool ready: false

    function colorOf(id: string, index: int): string {
        return root.colors[id] ?? root.palette[index % root.palette.length];
    }

    readonly property var sources: {
        const all = [{ id: "local", name: root.names["local"] ?? "My calendar", color: colorOf("local", 0) }].concat(Agenda.sources.map((url, i) => ({ id: url, name: root.names[url] ?? Agenda.names[url] ?? root.sourceName(url), color: colorOf(url, i + 1) })));
        const rank = id => {
            const i = root.order.indexOf(id);
            return i < 0 ? root.order.length : i;
        };
        return all.map((src, i) => ({ src, i })).sort((a, b) => rank(a.src.id) - rank(b.src.id) || a.i - b.i).map(o => o.src);
    }

    readonly property var events: {
        const hiddenSet = root.hidden;
        const colors = root.colors;
        const mine = root.local.map(e => Object.assign({}, e, { source: "local", color: root.colorOf("local", 0), day: root.dayKey(e.start) }));
        const remote = Agenda.events.map(e => Object.assign({}, e, { source: Agenda.sources[e.source] ?? "", color: root.colorOf(Agenda.sources[e.source] ?? "", e.source + 1), readonly: true }));
        return mine.concat(remote).filter(e => !hiddenSet.includes(e.source)).sort((a, b) => a.start - b.start || (a.allDay === b.allDay ? 0 : a.allDay ? -1 : 1));
    }

    readonly property var upcoming: {
        const start = new Date();
        start.setHours(0, 0, 0, 0);
        return events.filter(e => e.end > start.getTime()).slice(0, 80);
    }

    readonly property var results: {
        const tokens = root.query.trim().toLowerCase().split(/\s+/).filter(t => t.length > 0);
        if (tokens.length === 0)
            return [];
        const names = {};
        for (const src of root.sources)
            names[src.id] = src.name.toLowerCase();
        const now = Date.now();
        const out = [];
        for (const e of root.events) {
            const summary = e.summary.toLowerCase();
            const fields = [(e.location ?? "").toLowerCase(), names[e.source] ?? "", (e.notes ?? "").toLowerCase()];
            let score = 0;
            let hits = [];
            for (const t of tokens) {
                const m = root.fuzzy(t, summary);
                let best = m ? m.score * 1.5 : 0;
                for (const f of fields) {
                    const fm = root.fuzzy(t, f);
                    if (fm && fm.score > best)
                        best = fm.score;
                }
                if (best === 0) {
                    score = 0;
                    break;
                }
                if (m)
                    hits = hits.concat(m.hits);
                score += best;
            }
            if (score > 0)
                out.push(Object.assign({}, e, { score, hits, upcoming: e.end >= now }));
        }
        out.sort((a, b) => b.score - a.score || (a.upcoming !== b.upcoming ? (a.upcoming ? -1 : 1) : a.upcoming ? a.start - b.start : b.start - a.start));
        return out.slice(0, 60);
    }

    function fuzzy(q: string, s: string): var {
        if (s.length === 0)
            return null;
        const at = s.indexOf(q);
        if (at >= 0) {
            const boundary = at === 0 || /[\s\-_:/(.]/.test(s[at - 1]);
            return { score: 100 + (boundary ? 40 : 0) + (at === 0 ? 20 : 0) + (q.length === s.length ? 40 : 0) - Math.min(at, 20), hits: Array.from({ length: q.length }, (_, i) => at + i) };
        }
        const hits = [];
        let score = 0;
        let last = -2;
        let j = 0;
        for (let i = 0; i < s.length && j < q.length; i++) {
            if (s[i] !== q[j])
                continue;
            const boundary = i === 0 || /[\s\-_:/(.]/.test(s[i - 1]);
            score += (i === last + 1 ? 8 : 0) + (boundary ? 10 : 0) + 2 - Math.min(i - last - 1, 6);
            hits.push(i);
            last = i;
            j++;
        }
        if (j < q.length || score < q.length * 3)
            return null;
        return { score: Math.min(90, score), hits };
    }

    function dayKey(ms: real): string {
        return Qt.formatDate(new Date(ms), "yyyy-MM-dd");
    }

    function sourceName(url: string): string {
        try {
            const host = url.replace(/^https?:\/\//, "").split("/")[0];
            return host.replace(/^(p\d+-)?(caldav|calendar)\./, "").replace(/\.com$|\.org$/, "");
        } catch (e) {
            return url;
        }
    }

    function eventsOn(d: date): var {
        const key = Qt.formatDate(d, "yyyy-MM-dd");
        return root.events.filter(e => e.day === key);
    }

    function hasEvents(d: date): bool {
        const key = Qt.formatDate(d, "yyyy-MM-dd");
        return root.events.some(e => e.day === key);
    }

    function eventsBetween(a: real, b: real): var {
        return root.events.filter(e => e.start < b && e.end > a);
    }

    function setColor(id: string, color: string): void {
        const next = Object.assign({}, root.colors);
        next[id] = color;
        root.colors = next;
        root.persist();
    }

    function rename(id: string, name: string): void {
        const next = Object.assign({}, root.names);
        if (name.trim().length > 0)
            next[id] = name.trim();
        else
            delete next[id];
        root.names = next;
        root.persist();
    }

    function moveSource(id: string, to: int): void {
        const ids = root.sources.map(src => src.id);
        const from = ids.indexOf(id);
        if (from < 0)
            return;
        ids.splice(from, 1);
        ids.splice(to > from ? to - 1 : to, 0, id);
        root.order = ids;
        root.persist();
    }

    function cycleView(dir: int): void {
        const views = ["month", "week", "day"];
        root.view = views[(views.indexOf(root.view) + dir + 3) % 3];
    }

    function toggleSource(id: string): void {
        root.hidden = root.hidden.includes(id) ? root.hidden.filter(h => h !== id) : root.hidden.concat([id]);
        root.persist();
    }

    function show(d: date): void {
        root.cursor = d;
        root.selected = d;
        root.open = true;
    }

    function goto(d: date): void {
        root.cursor = d;
        root.selected = d;
    }

    function step(dir: int): void {
        const c = new Date(root.cursor);
        if (root.view === "month")
            c.setMonth(c.getMonth() + dir);
        else if (root.view === "week")
            c.setDate(c.getDate() + dir * 7);
        else
            c.setDate(c.getDate() + dir);
        root.cursor = c;
        if (root.view !== "month")
            root.selected = c;
    }

    function today(): void {
        root.goto(new Date());
    }

    function draft(start: real, allDay: bool): void {
        const s = new Date(start);
        const e = new Date(start);
        if (allDay) {
            s.setHours(0, 0, 0, 0);
            e.setTime(s.getTime() + 86400000);
        } else
            e.setHours(s.getHours() + 1);
        root.editing = { id: 0, summary: "", start: s.getTime(), end: e.getTime(), allDay, location: "", notes: "" };
    }

    function edit(ev: var): void {
        if (ev.readonly) {
            root.editing = Object.assign({}, ev);
            return;
        }
        root.editing = { id: ev.id, summary: ev.summary, start: ev.start, end: ev.end, allDay: ev.allDay, location: ev.location ?? "", notes: ev.notes ?? "" };
    }

    function save(ev: var): void {
        if (ev.summary.trim().length === 0)
            ev.summary = "(untitled)";
        if (ev.end <= ev.start)
            ev.end = ev.start + (ev.allDay ? 86400000 : 1800000);
        const clean = { id: ev.id || ++root.counter, summary: ev.summary.trim(), start: ev.start, end: ev.end, allDay: !!ev.allDay, location: ev.location ?? "", notes: ev.notes ?? "" };
        root.local = root.local.filter(e => e.id !== clean.id).concat([clean]);
        root.editing = null;
        root.persist();
    }

    function remove(id: int): void {
        root.local = root.local.filter(e => e.id !== id);
        if (root.editing?.id === id)
            root.editing = null;
        root.persist();
    }

    function move(id: int, delta: real): void {
        root.local = root.local.map(e => e.id === id ? Object.assign({}, e, { start: e.start + delta, end: e.end + delta }) : e);
        root.persist();
    }

    onViewChanged: persist()

    function persist(): void {
        if (root.ready)
            save.restart();
    }

    function load(text: string): void {
        try {
            const data = JSON.parse(text);
            root.local = data.events ?? [];
            root.hidden = data.hidden ?? [];
            root.colors = data.colors ?? {};
            root.names = data.names ?? {};
            root.order = data.order ?? [];
            if (["month", "week", "day"].includes(data.view))
                root.view = data.view;
            root.counter = data.counter ?? root.local.reduce((m, e) => Math.max(m, e.id), 0);
        } catch (e) {}
        root.ready = true;
    }

    FileView {
        id: file
        path: SpotlightConfig.stateDir + "/calendar.json"
        printErrors: false
        onLoaded: root.load(text())
        onLoadFailed: root.load("")
    }

    Timer {
        id: save
        interval: 300
        onTriggered: file.setText(JSON.stringify({ events: root.local, hidden: root.hidden, colors: root.colors, names: root.names, order: root.order, view: root.view, counter: root.counter }))
    }
}
