pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io
import "spotlight"
import "../config"

Singleton {
    id: root

    readonly property var icons: ({
            "Recent": "history",
            "Smileys & Emotion": "mood",
            "People & Body": "face",
            "Animals & Nature": "pets",
            "Food & Drink": "restaurant",
            "Travel & Places": "flight",
            "Activities": "cake",
            "Objects": "lightbulb_outline",
            "Symbols": "favorite_border",
            "Flags": "flag"
        })
    readonly property var tones: ["✋", "✋🏻", "✋🏼", "✋🏽", "✋🏾", "✋🏿"]

    property bool open: false
    property var groups: []
    property var all: []
    property var index: ({})
    property var vocab: []
    property var emoticons: ({})
    property var recent: []
    property var usage: ({})
    property var learned: ({})
    property int tone: 0
    property string query: ""
    property int category: 1

    readonly property var categories: ["Recent"].concat(groups).map(name => ({ name: name, icon: icons[name] ?? "mood" }))
    readonly property var results: {
        const q = root.query.trim();
        if (q)
            return root.search(q);
        if (root.category === 0)
            return root.recent.map(c => root.all.find(e => e.char === c)).filter(e => e);
        return root.all.filter(e => e.group === root.category - 1);
    }

    function toggle(): void {
        if (root.open)
            root.open = false;
        else
            root.show();
    }

    function show(): void {
        root.query = "";
        root.category = root.recent.length > 0 ? 0 : 1;
        root.open = true;
    }

    function cycle(delta: int): void {
        const n = root.categories.length;
        root.query = "";
        root.category = (root.category + delta + n) % n;
    }

    function cycleTone(): void {
        root.tone = (root.tone + 1) % root.tones.length;
        save.restart();
    }

    function glyph(entry: var): string {
        return root.tone > 0 && entry.skins ? entry.skins[root.tone - 1] : entry.char;
    }

    function pick(entry: var, copy: bool): void {
        if (!entry)
            return;
        const char = root.glyph(entry);
        const q = root.query.trim().toLowerCase();
        root.recent = [entry.char].concat(root.recent.filter(c => c !== entry.char)).slice(0, 36);
        root.usage[entry.char] = [(root.usage[entry.char]?.[0] ?? 0) + 1, Date.now()];
        if (q) {
            const seen = root.learned[q] ?? {};
            seen[entry.char] = (seen[entry.char] ?? 0) + 1;
            root.learned[q] = seen;
        }
        save.restart();
        root.open = false;
        if (copy)
            Quickshell.execDetached(["wl-copy", char]);
        else
            Quickshell.execDetached(["sh", "-c", "sleep 0.12; wtype -- \"$1\"", "sh", char]);
    }

    function stem(w: string): string {
        if (w.length > 5 && w.endsWith("ing"))
            return w.slice(0, -3);
        if (w.length > 4 && w.endsWith("ies"))
            return w.slice(0, -3) + "y";
        if (w.length > 4 && w.endsWith("ed"))
            return w.slice(0, -2);
        if (w.length > 3 && w.endsWith("s") && !w.endsWith("ss"))
            return w.slice(0, -1);
        return w;
    }

    function terms(text: string): var {
        return text.toLowerCase().normalize("NFD").replace(/[̀-ͯ]/g, "").split(/[^a-z0-9+\-']+/).filter(t => t);
    }

    function load(text: string): void {
        const data = JSON.parse(text);
        const all = [];
        const index = {};
        const emoticons = {};
        const add = (token, i, weight, field) => {
            const hits = index[token] ?? (index[token] = {});
            const hit = hits[i] ?? (hits[i] = { best: 0, fields: 0 });
            hit.best = Math.max(hit.best, weight);
            hit.fields |= 1 << field;
        };
        data.emoji.forEach((row, i) => {
            const entry = {
                char: row[0],
                group: row[1],
                name: row[2],
                codes: row[5] ? row[5].split(" ") : [],
                skins: row[7] || null,
                popular: (row[8] || 0) / 100,
                order: i
            };
            all.push(entry);
            const fields = [[row[2], 0.95], [row[5], 0.95], [row[3], 0.9], [row[4], 0.85]];
            fields.forEach(([text, weight], field) => {
                for (const t of root.terms(text)) {
                    add(t, i, weight, field);
                    const st = root.stem(t);
                    if (st !== t)
                        add(st, i, weight * 0.95, field);
                }
            });
            for (const code of entry.codes)
                add(code.replace(/_/g, ""), i, 0.9, 1);
            for (const e of row[6] ? row[6].split(" ") : [])
                (emoticons[e] ?? (emoticons[e] = [])).push(i);
        });
        for (const token in index) {
            const hits = index[token];
            for (const i in hits) {
                let n = 0;
                for (let f = hits[i].fields; f; f >>= 1)
                    n += f & 1;
                hits[i] = hits[i].best + 0.1 * (n - 1);
            }
        }
        root.groups = data.groups;
        root.all = all;
        root.index = index;
        root.vocab = Object.keys(index).sort();
        root.emoticons = emoticons;
    }

    function lowerBound(prefix: string): int {
        let lo = 0;
        let hi = root.vocab.length;
        while (lo < hi) {
            const mid = (lo + hi) >> 1;
            if (root.vocab[mid] < prefix)
                lo = mid + 1;
            else
                hi = mid;
        }
        return lo;
    }

    function match(term: string): var {
        const scores = {};
        const hit = (token, quality) => {
            const hits = root.index[token];
            for (const i in hits) {
                const s = hits[i] * quality;
                if ((scores[i] ?? 0) < s)
                    scores[i] = s;
            }
        };
        hit(term, 1);
        hit(root.stem(term), 0.95);
        for (let k = root.lowerBound(term), n = 0; k < root.vocab.length && n < 400 && root.vocab[k].startsWith(term); k++, n++)
            if (root.vocab[k] !== term)
                hit(root.vocab[k], 0.5 + 0.3 * term.length / root.vocab[k].length);
        if (Object.keys(scores).length < 4 && term.length >= 4) {
            const max = term.length >= 7 ? 2 : 1;
            for (const token of root.vocab)
                if (Math.abs(token.length - term.length) <= max && Fuzzy.levenshtein(term, token, max) <= max)
                    hit(token, 0.55);
        }
        return scores;
    }

    function boost(entry: var, q: string): real {
        let s = 0.3 * entry.popular;
        const own = root.learned[q]?.[entry.char] ?? 0;
        if (own)
            s += 0.8 * Math.log(1 + own);
        for (const key in root.learned)
            if (key !== q && key.startsWith(q) && root.learned[key][entry.char])
                s += 0.25 * Math.log(1 + root.learned[key][entry.char]);
        const use = root.usage[entry.char];
        if (use)
            s += 0.15 * Math.log(1 + use[0]) * Math.pow(0.5, (Date.now() - use[1]) / (30 * 86400000));
        return s;
    }

    function byCode(c: string): var {
        const scores = {};
        root.all.forEach((e, i) => {
            for (const x of e.codes) {
                const s = x === c ? 3 : x.startsWith(c) ? 2 + c.length / x.length : c.length >= 2 && x.includes(c) ? 1 + c.length / x.length : 0;
                if (s > (scores[i] ?? 0))
                    scores[i] = s;
            }
        });
        return scores;
    }

    function byTerms(q: string): var {
        const scores = {};
        const words = root.terms(q);
        const per = words.map(w => root.match(w));
        let pool = words.length ? Object.keys(per[0]).filter(i => per.every(p => p[i] !== undefined)) : [];
        const strict = pool.length > 0;
        if (!strict)
            pool = [...new Set(per.flatMap(p => Object.keys(p)))];
        for (const i of pool) {
            const e = root.all[i];
            let s = per.reduce((sum, p) => sum + (p[i] ?? 0), 0) / words.length * (strict ? 1 : 0.7);
            if (e.name === q)
                s += 1;
            else if (e.name.startsWith(q) && q.length * 2 >= e.name.length)
                s += 0.4 * q.length / e.name.length;
            if (e.codes.includes(q.replace(/ /g, "_")))
                s += 0.25;
            scores[i] = s;
        }
        return scores;
    }

    function search(raw: string): var {
        const q = raw.toLowerCase();
        const exact = root.emoticons[raw] ?? root.emoticons[q] ?? root.emoticons[raw.toUpperCase()];
        if (exact)
            return exact.map(i => root.all[i]);
        const code = raw.match(/^:([a-z0-9_+\-]+):?$/i);
        const scores = code ? root.byCode(code[1].toLowerCase()) : root.byTerms(q);
        const out = [];
        for (const i in scores)
            out.push({ entry: root.all[i], score: scores[i] + root.boost(root.all[i], q) });
        out.sort((a, b) => b.score - a.score || a.entry.order - b.entry.order);
        return out.slice(0, 240).map(r => r.entry);
    }

    FileView {
        path: Qt.resolvedUrl("../assets/emoji.json")
        onLoaded: root.load(text())
    }

    FileView {
        id: state
        path: SpotlightConfig.stateDir + "/emoji.json"
        printErrors: false
        onLoaded: {
            try {
                const data = JSON.parse(text());
                root.recent = data.recent ?? [];
                root.usage = data.usage ?? {};
                root.learned = data.learned ?? {};
                root.tone = data.tone ?? 0;
            } catch (e) {}
        }
    }

    Timer {
        id: save
        interval: 300
        onTriggered: state.setText(JSON.stringify({ recent: root.recent, usage: root.usage, learned: root.learned, tone: root.tone }))
    }
}
