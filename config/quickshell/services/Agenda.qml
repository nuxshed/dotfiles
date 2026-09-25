pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io
import "../config"

Singleton {
    id: root

    readonly property int pastDays: 31
    readonly property int futureDays: 92
    readonly property string separator: "===QS-ICS==="
    readonly property string cacheDir: SpotlightConfig.stateDir + "/ics"

    property var sources: []
    property var events: []
    property var names: ({})
    property bool ready: false
    property bool loading: false
    property bool queued: false

    function dayKey(ms: real): string {
        return Qt.formatDate(new Date(ms), "yyyy-MM-dd");
    }

    function add(url: string): void {
        let u = url.trim().replace(/^webcal:\/\//i, "https://");
        if (!/^https?:\/\//i.test(u) || root.sources.includes(u))
            return;
        root.sources = root.sources.concat([u]);
        root.persist();
        root.refresh();
    }

    function remove(url: string): void {
        Quickshell.execDetached(["bash", "-c", `rm -f ${JSON.stringify(root.cacheDir)}/$(printf %s ${JSON.stringify(url)} | md5sum | cut -c1-32).ics`]);
        root.sources = root.sources.filter(s => s !== url);
        root.persist();
        root.refresh();
    }

    function script(online: bool): var {
        const body = root.sources.map(u => {
            const url = JSON.stringify(u);
            const get = online ? `curl -fsSL --max-time 20 ${url} -o "$f.tmp" && grep -q BEGIN:VCALENDAR "$f.tmp" && mv "$f.tmp" "$f"; rm -f "$f.tmp"; ` : "";
            return `f="$d/$(printf %s ${url} | md5sum | cut -c1-32).ics"; ${get}cat "$f" 2>/dev/null; echo; echo ${root.separator}`;
        }).join("; ");
        return ["bash", "-c", `d=${JSON.stringify(root.cacheDir)}; mkdir -p "$d"; ${body}`];
    }

    function refresh(): void {
        if (root.sources.length === 0) {
            root.events = [];
            return;
        }
        if (fetch.running) {
            root.queued = true;
            return;
        }
        fetch.command = root.script(true);
        root.loading = true;
        fetch.running = true;
    }

    function parseDate(value: string, params: string): var {
        const allDay = /VALUE=DATE(?!-TIME)/.test(params) || value.length === 8;
        const y = +value.slice(0, 4), mo = +value.slice(4, 6) - 1, d = +value.slice(6, 8);
        if (allDay)
            return { ms: new Date(y, mo, d).getTime(), allDay: true };
        const h = +value.slice(9, 11), mi = +value.slice(11, 13), s = +value.slice(13, 15) || 0;
        const ms = value.endsWith("Z") ? Date.UTC(y, mo, d, h, mi, s) : new Date(y, mo, d, h, mi, s).getTime();
        return { ms, allDay: false };
    }

    function unfold(text: string): var {
        return text.replace(/\r\n?/g, "\n").replace(/\n[ \t]/g, "").split("\n");
    }

    function parseRule(text: string): var {
        const rule = {};
        for (const part of text.split(";")) {
            const [k, v] = part.split("=");
            rule[k] = v;
        }
        return rule;
    }

    function expand(start: real, rule: var, exdates: var, windowStart: real, windowEnd: real): var {
        const out = [];
        const interval = +(rule.INTERVAL ?? 1);
        const count = rule.COUNT ? +rule.COUNT : Infinity;
        const until = rule.UNTIL ? root.parseDate(rule.UNTIL, "").ms : Infinity;
        const dayNames = ["SU", "MO", "TU", "WE", "TH", "FR", "SA"];
        const byDay = rule.BYDAY ? rule.BYDAY.split(",").map(d => dayNames.indexOf(d.slice(-2))) : null;
        const first = new Date(start);
        let n = 0;
        let i = 0;

        const push = ms => {
            if (ms > until || n >= count)
                return false;
            n++;
            if (ms >= windowStart && ms <= windowEnd && !exdates.includes(ms))
                out.push(ms);
            return true;
        };

        while (i < 2000) {
            const d = new Date(first);
            if (rule.FREQ === "DAILY")
                d.setDate(first.getDate() + i * interval);
            else if (rule.FREQ === "WEEKLY")
                d.setDate(first.getDate() + i * interval * 7);
            else if (rule.FREQ === "MONTHLY")
                d.setMonth(first.getMonth() + i * interval);
            else if (rule.FREQ === "YEARLY")
                d.setFullYear(first.getFullYear() + i * interval);
            else
                break;
            i++;
            if (d.getTime() > windowEnd || d.getTime() > until || n >= count)
                break;

            if (rule.FREQ === "WEEKLY" && byDay) {
                const weekStart = new Date(d);
                weekStart.setDate(d.getDate() - d.getDay());
                for (const wd of byDay.sort()) {
                    const inst = new Date(weekStart);
                    inst.setDate(weekStart.getDate() + wd);
                    if (inst.getTime() < start)
                        continue;
                    if (!push(inst.getTime()))
                        return out;
                }
            } else if (!push(d.getTime()))
                return out;
        }
        return out;
    }

    function parse(text: string, source: int): var {
        const out = [];
        const now = new Date();
        const windowStart = new Date(now.getFullYear(), now.getMonth(), now.getDate() - root.pastDays).getTime();
        const windowEnd = new Date(now.getFullYear(), now.getMonth(), now.getDate() + root.futureDays).getTime();
        let ev = null;

        for (const line of root.unfold(text)) {
            if (line === "BEGIN:VEVENT") {
                ev = { exdates: [] };
                continue;
            }
            if (line === "END:VEVENT" && ev) {
                if (ev.start !== undefined) {
                    const duration = ev.end !== undefined ? ev.end - ev.start : (ev.allDay ? 86400000 : 0);
                    const starts = ev.rrule ? root.expand(ev.start, ev.rrule, ev.exdates, windowStart, windowEnd)
                        : (ev.start + duration >= windowStart && ev.start <= windowEnd ? [ev.start] : []);
                    for (const s of starts)
                        out.push({ start: s, end: s + duration, day: root.dayKey(s), allDay: ev.allDay, summary: ev.summary ?? "(untitled)", location: ev.location ?? "", source });
                }
                ev = null;
                continue;
            }
            if (!ev)
                continue;
            const idx = line.indexOf(":");
            if (idx < 0)
                continue;
            const head = line.slice(0, idx);
            const value = line.slice(idx + 1);
            const semi = head.indexOf(";");
            const name = semi < 0 ? head : head.slice(0, semi);
            const params = semi < 0 ? "" : head.slice(semi + 1);

            if (name === "DTSTART") {
                const d = root.parseDate(value, params);
                ev.start = d.ms;
                ev.allDay = d.allDay;
            } else if (name === "DTEND")
                ev.end = root.parseDate(value, params).ms;
            else if (name === "SUMMARY")
                ev.summary = value.replace(/\\,/g, ",").replace(/\\n/g, " ").replace(/\\\\/g, "\\");
            else if (name === "LOCATION")
                ev.location = value.replace(/\\,/g, ",");
            else if (name === "RRULE")
                ev.rrule = root.parseRule(value);
            else if (name === "EXDATE")
                for (const v of value.split(","))
                    ev.exdates.push(root.parseDate(v, params).ms);
        }
        return out;
    }

    function ingest(text: string, fresh: bool): void {
        const chunks = text.split(root.separator);
        let all = [];
        const names = {};
        chunks.forEach((chunk, i) => {
            if (!chunk.includes("BEGIN:VCALENDAR"))
                return;
            const m = chunk.match(/^X-WR-CALNAME:(.+)$/m);
            if (m && root.sources[i])
                names[root.sources[i]] = m[1].trim().replace(/\\,/g, ",");
            all = all.concat(root.parse(chunk, i));
        });
        root.names = names;
        all.sort((a, b) => a.start - b.start || (a.allDay ? -1 : 1));
        root.events = all;
        if (fresh)
            root.loading = false;
    }

    function persist(): void {
        if (root.ready)
            file.setText(JSON.stringify({ sources: root.sources }));
    }

    function load(text: string): void {
        try {
            root.sources = JSON.parse(text).sources ?? [];
        } catch (e) {}
        root.ready = true;
        if (root.sources.length > 0) {
            cached.command = root.script(false);
            cached.running = true;
        }
        root.refresh();
    }

    Process {
        id: cached
        stdout: StdioCollector {
            onStreamFinished: if (root.loading)
                root.ingest(text, false)
        }
    }

    Process {
        id: fetch
        stdout: StdioCollector {
            onStreamFinished: root.ingest(text, true)
        }
        onExited: if (root.queued) {
            root.queued = false;
            root.refresh();
        }
    }

    Timer {
        interval: 15 * 60 * 1000
        running: root.sources.length > 0
        repeat: true
        onTriggered: root.refresh()
    }

    FileView {
        id: file
        path: SpotlightConfig.stateDir + "/agenda.json"
        printErrors: false
        onLoaded: root.load(text())
        onLoadFailed: root.load("")
    }
}
