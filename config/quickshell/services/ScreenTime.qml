pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io
import Quickshell.Wayland
import "../config"

Singleton {
    id: root

    readonly property int idleTimeout: 120
    readonly property int keepDays: 365
    readonly property int keepHours: 90
    readonly property string shellPrefix: "org.quickshell/"

    readonly property var categoryList: [
        { id: "web", name: "Web", color: Colors.blue },
        { id: "social", name: "Social", color: Colors.magenta },
        { id: "dev", name: "Development", color: Colors.green },
        { id: "work", name: "Productivity", color: Colors.yellow },
        { id: "media", name: "Entertainment", color: Colors.red },
        { id: "creative", name: "Creativity", color: Colors.cyan },
        { id: "other", name: "Other", color: Colors.outline }
    ]

    property bool appOpen: false
    property string view: "day"
    property string cursor: ""
    property var days: ({})
    property var categories: ({})
    property int revision: 0
    property bool ready: false
    property real now: Date.now()
    property real beat: Date.now()

    property string spanApp: ""
    property string spanDetail: ""
    property real spanStart: 0
    property string detail: ""
    property var labels: ({})
    property bool pending: false
    property real streakStart: Date.now()
    property bool wasAway: false

    readonly property var entries: DesktopEntries.applications.values
    readonly property bool away: idle.isIdle || Lock.locked
    readonly property string app: {
        const t = ToplevelManager.activeToplevel;
        if (!t || !t.appId)
            return "";
        return t.appId === "org.quickshell" ? root.shellPrefix + (t.title || "Shell") : t.appId;
    }
    readonly property string title: ToplevelManager.activeToplevel?.title ?? ""
    readonly property bool browser: /^(zen|firefox)/.test(app)
    readonly property bool detailed: browser || app === "org.wezfurlong.wezterm"
    readonly property string today: key(new Date(now))
    readonly property var todayStats: day(today)

    function key(d: var): string {
        return Qt.formatDate(new Date(d), "yyyy-MM-dd");
    }

    function dateOf(k: string): date {
        const p = k.split("-");
        return new Date(+p[0], +p[1] - 1, +p[2]);
    }

    function shift(k: string, n: int): string {
        const d = dateOf(k);
        d.setDate(d.getDate() + n);
        return key(d);
    }

    function format(seconds: real): string {
        const m = Math.floor(seconds / 60);
        if (m < 1)
            return seconds > 0 ? "<1m" : "0m";
        return m < 60 ? `${m}m` : m % 60 === 0 ? `${m / 60}h` : `${Math.floor(m / 60)}h ${m % 60}m`;
    }

    function clock(ms: real): string {
        return ms > 0 ? Qt.formatTime(new Date(ms), Settings.clock24 ? "HH:mm" : "h:mm AP") : "–";
    }

    function lookup(id: string): var {
        root.entries;
        return id.startsWith(root.shellPrefix) ? null : DesktopEntries.heuristicLookup(id);
    }

    function group(id: string): string {
        return root.lookup(id)?.id ?? id;
    }

    function detect(entry: var): string {
        const c = entry?.categories ?? [];
        const has = list => list.some(x => c.includes(x));
        if (has(["WebBrowser"]))
            return "web";
        if (has(["InstantMessaging", "Chat", "IRCClient", "VideoConference", "Telephony"]))
            return "social";
        if (has(["Development", "IDE", "TerminalEmulator", "Debugger", "RevisionControl"]))
            return "dev";
        if (has(["AudioVideo", "Audio", "Video", "Player", "Game", "Music"]))
            return "media";
        if (has(["Graphics", "Photography", "2DGraphics", "3DGraphics", "RasterGraphics", "VectorGraphics"]))
            return "creative";
        if (has(["Office", "Email", "Education", "Science", "Calendar", "TextEditor", "WordProcessor", "Finance"]))
            return "work";
        return "other";
    }

    function info(id: string): var {
        root.categories;
        const entry = root.lookup(id);
        const shell = id.startsWith(root.shellPrefix);
        return {
            id,
            name: shell ? id.slice(root.shellPrefix.length) : entry?.name ?? id,
            icon: shell ? "" : entry?.icon ?? id.toLowerCase(),
            shell,
            category: root.categories[id] ?? (shell ? "work" : root.detect(entry))
        };
    }

    function categoryOf(id: string): var {
        return root.categoryList.find(c => c.id === id) ?? root.categoryList[root.categoryList.length - 1];
    }

    function categoryFor(id: string, label: string): string {
        return (label && root.categories["@" + label]) || root.info(id).category;
    }

    function setLabelCategory(id: string, label: string, category: string): void {
        const next = Object.assign({}, root.categories);
        if (category === root.info(id).category)
            delete next["@" + label];
        else
            next["@" + label] = category;
        root.categories = next;
        root.persist();
    }

    function setCategory(id: string, category: string): void {
        const next = Object.assign({}, root.categories);
        if (category === root.detect(root.lookup(id)))
            delete next[id];
        else
            next[id] = category;
        root.categories = next;
        root.persist();
    }

    function slices(a: real, b: real, fn: var): void {
        let t = a;
        while (t < b) {
            const d = new Date(t);
            const edge = new Date(d.getFullYear(), d.getMonth(), d.getDate(), d.getHours() + 1).getTime();
            const end = Math.min(b, edge);
            fn(root.key(d), d.getHours(), (end - t) / 1000, t, end);
            t = end;
        }
    }

    function entry(k: string): var {
        if (!root.days[k])
            root.days[k] = { apps: {}, hours: {}, pickups: 0, longest: 0, first: 0, last: 0 };
        return root.days[k];
    }

    function commit(id: string, detail: string, a: real, b: real): void {
        if (!id || b - a < 1000)
            return;
        root.slices(a, b, (k, h, s, from, to) => {
            const d = root.entry(k);
            d.apps[id] = Math.round((d.apps[id] ?? 0) + s);
            if (detail) {
                d.details = d.details ?? {};
                d.details[id] = d.details[id] ?? {};
                d.details[id][detail] = Math.round((d.details[id][detail] ?? 0) + s);
            }
            const hk = detail ? id + "\t" + detail : id;
            d.hours[h] = d.hours[h] ?? {};
            d.hours[h][hk] = Math.round((d.hours[h][hk] ?? 0) + s);
            if (!d.first || from < d.first)
                d.first = from;
            d.last = Math.max(d.last, to);
        });
        root.revision++;
    }

    function endStreak(at: real): void {
        const s = (at - root.streakStart) / 1000;
        if (s > 0) {
            const d = root.entry(root.key(at));
            d.longest = Math.max(d.longest ?? 0, Math.round(s));
        }
    }

    function sync(): void {
        const t = Date.now();
        const gap = t - root.beat > 60000;
        const cut = gap ? root.beat : idle.isIdle ? Math.max(root.spanStart, t - root.idleTimeout * 1000) : t;
        const away = root.away || gap;
        root.beat = t;
        root.now = t;
        if (away && !root.wasAway)
            root.endStreak(cut);
        const target = root.away ? "" : root.app;
        const detail = target ? root.detail : "";
        if (target !== root.spanApp || detail !== root.spanDetail || gap) {
            root.commit(root.spanApp, root.spanDetail, root.spanStart, Math.max(root.spanStart, away ? cut : t));
            root.spanApp = target;
            root.spanDetail = detail;
            root.spanStart = t;
        }
        if ((!root.away && root.wasAway) || (gap && !root.away)) {
            root.streakStart = t;
            if (root.ready) {
                root.entry(root.key(t)).pickups++;
                root.revision++;
            }
        }
        root.wasAway = root.away;
    }

    function day(k: string): var {
        root.revision;
        root.now;
        root.categories;
        root.entries;
        const src = root.days[k] ?? {};
        const raw = Object.assign({}, src.apps ?? {});
        const rawDetails = {};
        for (const id of Object.keys(src.details ?? {}))
            rawDetails[id] = Object.assign({}, src.details[id]);
        const rawHours = {};
        for (const h of Object.keys(src.hours ?? {}))
            rawHours[h] = Object.assign({}, src.hours[h]);
        let first = src.first ?? 0;
        let last = src.last ?? 0;
        if (root.spanApp)
            root.slices(root.spanStart, root.now, (dk, h, s, from, to) => {
                if (dk !== k)
                    return;
                raw[root.spanApp] = (raw[root.spanApp] ?? 0) + s;
                if (root.spanDetail) {
                    rawDetails[root.spanApp] = rawDetails[root.spanApp] ?? {};
                    rawDetails[root.spanApp][root.spanDetail] = (rawDetails[root.spanApp][root.spanDetail] ?? 0) + s;
                }
                const hk = root.spanDetail ? root.spanApp + "\t" + root.spanDetail : root.spanApp;
                rawHours[h] = rawHours[h] ?? {};
                rawHours[h][hk] = (rawHours[h][hk] ?? 0) + s;
                first = first ? Math.min(first, from) : from;
                last = Math.max(last, to);
            });
        const apps = {};
        for (const id of Object.keys(raw)) {
            const g = root.group(id);
            apps[g] = (apps[g] ?? 0) + raw[id];
        }
        const details = {};
        for (const id of Object.keys(rawDetails)) {
            const g = root.group(id);
            details[g] = details[g] ?? {};
            for (const label of Object.keys(rawDetails[id]))
                details[g][label] = (details[g][label] ?? 0) + rawDetails[id][label];
        }
        const hours = [];
        const appHours = {};
        const cats = {};
        for (let h = 0; h < 24; h++) {
            const bucket = {};
            for (const hk of Object.keys(rawHours[h] ?? {})) {
                const parts = hk.split("\t");
                const g = root.group(parts[0]);
                const c = root.categoryFor(g, parts[1] ?? "");
                const s = rawHours[h][hk];
                bucket[c] = (bucket[c] ?? 0) + s;
                appHours[g] = appHours[g] ?? new Array(24).fill(0);
                appHours[g][h] += s;
            }
            hours.push(bucket);
        }
        const list = Object.keys(apps).map(id => ({ id, seconds: apps[id] })).sort((a, b) => b.seconds - a.seconds);
        const appCats = {};
        for (const a of list) {
            const own = root.info(a.id).category;
            const split = {};
            let rest = a.seconds;
            for (const label of Object.keys(details[a.id] ?? {})) {
                const c = root.categoryFor(a.id, label);
                if (c === own)
                    continue;
                const s = Math.min(rest, details[a.id][label]);
                split[c] = (split[c] ?? 0) + s;
                rest -= s;
            }
            split[own] = (split[own] ?? 0) + rest;
            appCats[a.id] = split;
            for (const c of Object.keys(split))
                cats[c] = (cats[c] ?? 0) + split[c];
        }
        const live = k === root.today && !root.away ? (root.now - root.streakStart) / 1000 : 0;
        return {
            key: k,
            total: list.reduce((s, a) => s + a.seconds, 0),
            apps: list,
            cats,
            hours,
            appHours,
            appCats,
            details,
            pickups: src.pickups ?? 0,
            longest: Math.max(src.longest ?? 0, live),
            first,
            last
        };
    }

    function range(from: string, count: int): var {
        const out = [];
        for (let i = 0; i < count; i++)
            out.push(root.day(root.shift(from, i)));
        return out;
    }

    function merge(list: var): var {
        const apps = {};
        const cats = {};
        const appHours = {};
        const details = {};
        const appCats = {};
        const hours = [];
        for (let h = 0; h < 24; h++)
            hours.push({});
        let pickups = 0;
        let longest = 0;
        for (const d of list) {
            for (const a of d.apps)
                apps[a.id] = (apps[a.id] ?? 0) + a.seconds;
            for (const c of Object.keys(d.cats))
                cats[c] = (cats[c] ?? 0) + d.cats[c];
            for (let h = 0; h < 24; h++)
                for (const c of Object.keys(d.hours[h]))
                    hours[h][c] = (hours[h][c] ?? 0) + d.hours[h][c];
            for (const id of Object.keys(d.appHours)) {
                appHours[id] = appHours[id] ?? new Array(24).fill(0);
                for (let h = 0; h < 24; h++)
                    appHours[id][h] += d.appHours[id][h];
            }
            for (const id of Object.keys(d.appCats)) {
                appCats[id] = appCats[id] ?? {};
                for (const c of Object.keys(d.appCats[id]))
                    appCats[id][c] = (appCats[id][c] ?? 0) + d.appCats[id][c];
            }
            for (const id of Object.keys(d.details)) {
                details[id] = details[id] ?? {};
                for (const label of Object.keys(d.details[id]))
                    details[id][label] = (details[id][label] ?? 0) + d.details[id][label];
            }
            pickups += d.pickups;
            longest = Math.max(longest, d.longest);
        }
        const sorted = Object.keys(apps).map(id => ({ id, seconds: apps[id] })).sort((a, b) => b.seconds - a.seconds);
        return {
            total: sorted.reduce((s, a) => s + a.seconds, 0),
            apps: sorted,
            cats,
            hours,
            appHours,
            appCats,
            details,
            pickups,
            longest,
            active: list.filter(d => d.total > 0).length
        };
    }

    function breakdown(agg: var, id: string, total: real): var {
        const map = agg.details[id] ?? {};
        const list = Object.keys(map).map(label => ({ label, seconds: map[label] })).sort((a, b) => b.seconds - a.seconds);
        const rest = total - list.reduce((s, d) => s + d.seconds, 0);
        if (list.length > 0 && rest >= 60)
            list.push({ label: "Unlabelled", seconds: rest, rest: true });
        return list;
    }

    function resolve(): void {
        if (!root.detailed) {
            root.detail = "";
            return;
        }
        if (resolver.running) {
            root.pending = true;
            return;
        }
        resolver.forApp = root.app;
        resolver.forTitle = root.title;
        resolver.command = [Quickshell.env("HOME") + "/dotfiles/bin/qs-appdetail", root.app, root.title];
        resolver.running = true;
    }

    function resolved(app: string, title: string, label: string): void {
        if (label && /^(zen|firefox)/.test(app)) {
            const next = Object.assign({}, root.labels);
            next[app + "\n" + title] = label;
            root.labels = next;
        }
        if (app === root.app && title === root.title)
            root.detail = label;
    }

    function show(k: string, v: string): void {
        root.cursor = k || root.today;
        if (v)
            root.view = v;
        root.appOpen = true;
    }

    function average(before: string, count: int): real {
        const list = root.range(root.shift(before, -count), count).filter(d => d.total > 0);
        return list.length > 0 ? list.reduce((s, d) => s + d.total, 0) / list.length : 0;
    }

    function usual(k: string, at: real): real {
        const t = new Date(at);
        const frac = (t.getMinutes() * 60 + t.getSeconds()) / 3600;
        const sum = b => Object.values(b).reduce((s, v) => s + v, 0);
        const list = root.range(root.shift(k, -7), 7).filter(d => d.total > 0 && d.hours.some(b => sum(b) > 0));
        if (list.length === 0)
            return -1;
        let total = 0;
        for (const d of list) {
            for (let h = 0; h < t.getHours(); h++)
                total += sum(d.hours[h]);
            total += sum(d.hours[t.getHours()]) * frac;
        }
        return total / list.length;
    }

    function prune(): void {
        const dayCut = root.shift(root.key(Date.now()), -root.keepDays);
        const hourCut = root.shift(root.key(Date.now()), -root.keepHours);
        for (const k of Object.keys(root.days)) {
            if (k < dayCut)
                delete root.days[k];
            else if (k < hourCut)
                root.days[k].hours = {};
        }
    }

    function load(text: string): void {
        let data = {};
        try {
            data = JSON.parse(text) ?? {};
        } catch (e) {}
        if (data.version === 2) {
            root.days = data.days ?? {};
            root.categories = data.categories ?? {};
            const fresh = data.open && Date.now() - data.open.seen < 90000;
            if (data.open?.app)
                root.commit(data.open.app, data.open.detail ?? "", data.open.start, fresh ? Date.now() : Math.min(data.open.seen, Date.now()));
            if (fresh && data.streak)
                root.streakStart = data.streak;
        } else {
            const days = {};
            for (const k of Object.keys(data))
                days[k] = { apps: data[k], hours: {}, pickups: 0, longest: 0, first: 0, last: 0 };
            root.days = days;
        }
        root.prune();
        root.ready = true;
        root.revision++;
        root.persist();
    }

    function snapshot(): string {
        return JSON.stringify({
            version: 2,
            days: root.days,
            categories: root.categories,
            open: root.spanApp ? { app: root.spanApp, detail: root.spanDetail, start: root.spanStart, seen: root.now } : null,
            streak: root.away ? 0 : root.streakStart
        });
    }

    function persist(): void {
        if (root.ready)
            file.setText(root.snapshot());
    }

    onAppChanged: {
        root.detail = root.browser ? root.labels[root.app + "\n" + root.title] ?? "" : "";
        root.sync();
        lookup.restart();
    }
    onTitleChanged: if (root.detailed) {
        const hit = root.browser ? root.labels[root.app + "\n" + root.title] : undefined;
        if (hit)
            root.detail = hit;
        lookup.restart();
    }
    onDetailChanged: sync()
    onAwayChanged: sync()

    Component.onCompleted: {
        root.spanStart = Date.now();
        root.spanApp = root.away ? "" : root.app;
        lookup.restart();
    }

    Timer {
        id: lookup
        interval: 300
        onTriggered: root.resolve()
    }

    Process {
        id: resolver

        property string forApp: ""
        property string forTitle: ""

        stdout: StdioCollector {
            onStreamFinished: root.resolved(resolver.forApp, resolver.forTitle, text.trim())
        }

        onExited: if (root.pending) {
            root.pending = false;
            lookup.restart();
        }
    }

    IdleMonitor {
        id: idle
        timeout: root.idleTimeout
    }

    Timer {
        interval: 15000
        running: true
        repeat: true
        onTriggered: {
            root.sync();
            if (root.detailed && !root.browser)
                lookup.restart();
        }
    }

    Timer {
        interval: 60000
        running: root.ready
        repeat: true
        onTriggered: root.persist()
    }

    FileView {
        id: file
        path: SpotlightConfig.stateDir + "/screentime.json"
        printErrors: false
        blockLoading: true
        onLoaded: root.load(text())
        onLoadFailed: root.load("")
    }
}
