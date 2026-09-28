import QtQuick
import Quickshell
import "../../../services"
import ".."

Provider {
    id: root

    name: "events"
    label: "Calendar"
    prefix: "cal"
    icon: "event"
    keywords: ["calendar", "events", "agenda", "schedule"]
    weight: 1
    cap: 3

    readonly property var months: ["jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"]
    readonly property var eventWords: /\b(event|meeting|meet|call|lunch|dinner|breakfast|brunch|appointment|appt|standup|class|lecture|exam|interview|party|dentist|doctor|flight|deadline|birthday)\b/

    function explicitTime(s, T) {
        const re = new RegExp(`\\s(at\\s+|@\\s*)?${T}(?=\\s)`, "g")
        let m
        while ((m = re.exec(s)) !== null)
            if (m[1] || m[3] || m[4])
                return m
        return null
    }

    function parse(input) {
        let s = " " + input.toLowerCase().replace(/[,]/g, " ") + " "
        const now = new Date()
        const base = () => {
            const d = new Date(now)
            d.setHours(0, 0, 0, 0)
            return d
        }
        const addDays = (d, n) => {
            const x = new Date(d)
            x.setDate(x.getDate() + n)
            return x
        }
        const take = re => {
            const m = s.match(re)
            if (m)
                s = s.replace(m[0], " ")
            return m
        }
        const hour = (h, mer, bare) => {
            h = parseInt(h)
            if (mer === "pm")
                return h % 12 + 12
            if (mer === "am")
                return h % 12
            return bare && h >= 1 && h <= 7 ? h + 12 : h
        }

        let day = null
        let time = null
        let end = null
        let dur = null
        let m

        if ((m = take(/\sfor\s+(\d+(?:\.\d+)?)\s*(h|hrs?|hours?|m|mins?|minutes?)\s/)))
            dur = parseFloat(m[1]) * (m[2][0] === "h" ? 60 : 1)

        if (take(/\s(?:the\s+)?day\s+after\s+tomorrow\s/))
            day = addDays(base(), 2)
        else if (take(/\stoday\s/))
            day = base()
        else if (take(/\s(?:tomorrow|tmrw?|tmr)\s/))
            day = addDays(base(), 1)
        else if (take(/\stonight\s/)) {
            day = base()
            time = { h: 20, m: 0 }
        } else if ((m = take(/\sin\s+(\d+)\s+(days?|weeks?)\s/)))
            day = addDays(base(), parseInt(m[1]) * (m[2][0] === "w" ? 7 : 1))
        else if (take(/\snext\s+week\s/)) {
            const d = base()
            day = addDays(d, (8 - d.getDay()) % 7 || 7)
        } else if ((m = take(/\s(?:on\s+)?(this\s+|next\s+)?(monday|mon|tuesday|tues|tue|wednesday|wed|thursday|thurs|thur|thu|friday|fri|saturday|sat|sunday|sun)\s/))) {
            const d = base()
            const idx = ["sun", "mon", "tue", "wed", "thu", "fri", "sat"].indexOf(m[2].slice(0, 3))
            let diff = (idx - d.getDay() + 7) % 7
            if (diff === 0 && m[1]?.trim() === "next")
                diff = 7
            day = addDays(d, diff)
        } else {
            const mon = "(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)(?:uary|ruary|ch|il|e|y|ust|t|tember|ober|ember)?"
            m = take(new RegExp(`\\s(?:on\\s+)?(\\d{1,2})(?:st|nd|rd|th)?\\s+${mon}\\s`))
            let dd = m ? parseInt(m[1]) : 0
            let mm = m ? root.months.indexOf(m[2]) : -1
            if (!m && (m = take(new RegExp(`\\s(?:on\\s+)?${mon}\\s+(\\d{1,2})(?:st|nd|rd|th)?\\s`)))) {
                dd = parseInt(m[2])
                mm = root.months.indexOf(m[1])
            }
            if (m) {
                const d = new Date(now.getFullYear(), mm, dd)
                if (d < base())
                    d.setFullYear(d.getFullYear() + 1)
                day = d
            }
        }

        const T = "(\\d{1,2})(?:[:.](\\d{2}))?\\s*(am|pm)?"
        if ((m = take(new RegExp(`\\s(?:from\\s+|at\\s+|@\\s*)?${T}\\s*(?:-|–|to|till|until)\\s*${T}\\s`)))) {
            const mer = m[3] ?? m[6]
            time = { h: hour(m[1], mer, !m[2] && !mer), m: parseInt(m[2] ?? "0"), bare: !mer }
            end = { h: hour(m[4], m[6] ?? mer, !m[5] && !mer), m: parseInt(m[5] ?? "0") }
        } else if ((m = root.explicitTime(s, T))) {
            s = s.replace(m[0], " ")
            time = { h: hour(m[2], m[4], !m[3] && !m[4]), m: parseInt(m[3] ?? "0"), bare: !m[4] }
        } else {
            if ((m = take(/\s(?:at\s+)?(noon|midnight)\s/)))
                time = { h: m[1] === "noon" ? 12 : 0, m: 0 }
            else if (!time && (m = take(/\s(?:in\s+the\s+)?(morning|afternoon|evening)\s/)))
                time = { h: { morning: 9, afternoon: 14, evening: 18 }[m[1]], m: 0 }
        }

        if (time && (time.h > 23 || time.m > 59))
            return null
        return { day: day, time: time, end: end, dur: dur, rest: s.replace(/\s+/g, " ").trim() }
    }

    function title(rest, original) {
        let t = rest
            .replace(/^(please\s+)?(add|create|new|schedule|put|set\s+up|book)\s+/, "")
            .replace(/^(an?\s+)?(event|appointment)\s+(for\s+|called\s+|named\s+)?/, "")
            .replace(/\s+(to|on|in)\s+(my\s+)?(calendar|cal)$/, "")
            .replace(/^remind\s+me\s+(to|about)\s+/, "")
            .replace(/(\s+(at|on|from|by|the))+$/, "")
            .trim()
        if (!t)
            return ""
        const i = original.toLowerCase().indexOf(t)
        const cased = i >= 0 ? original.substr(i, t.length) : t
        return cased.charAt(0).toUpperCase() + cased.slice(1)
    }

    function dayLabel(d) {
        const today = new Date()
        today.setHours(0, 0, 0, 0)
        const diff = Math.round((new Date(d).setHours(0, 0, 0, 0) - today.getTime()) / 86400000)
        if (diff === 0)
            return "Today"
        if (diff === 1)
            return "Tomorrow"
        if (diff > 1 && diff < 7)
            return Qt.formatDate(new Date(d), "dddd")
        return Qt.formatDate(new Date(d), "ddd d MMM")
    }

    function when(e) {
        if (e.allDay)
            return root.dayLabel(e.start) + " · all day"
        return root.dayLabel(e.start) + " · " + Qt.formatTime(new Date(e.start), "HH:mm") + " – " + Qt.formatTime(new Date(e.end), "HH:mm")
    }

    function draft(text, scoped) {
        const p = root.parse(text)
        if (!p || (!p.day && !p.time))
            return null
        const name = root.title(p.rest, text)
        if (!name)
            return null
        if (!scoped && !(p.day && p.time) && !root.eventWords.test(text.toLowerCase()))
            return null

        const now = new Date()
        if (p.time?.bare && p.time.h >= 8 && p.time.h <= 11) {
            const am = new Date(now)
            am.setHours(p.time.h, p.time.m, 0, 0)
            if (/\b(dinner|party|movie|night|evening|drinks|date|concert|show|game|pub)\b/i.test(text) || (!p.day && am < now)) {
                p.time.h += 12
                if (p.end && p.end.h < 12)
                    p.end.h += 12
            }
        }
        let day = p.day
        if (!day) {
            day = new Date(now)
            day.setHours(0, 0, 0, 0)
            const at = new Date(day)
            at.setHours(p.time.h, p.time.m)
            if (at < now)
                day.setDate(day.getDate() + 1)
        }
        if (!p.time) {
            const start = day.getTime()
            return { summary: name, start: start, end: start + 86400000, allDay: true }
        }
        const s = new Date(day)
        s.setHours(p.time.h, p.time.m, 0, 0)
        let e
        if (p.end) {
            e = new Date(day)
            e.setHours(p.end.h, p.end.m, 0, 0)
            if (e <= s)
                e.setDate(e.getDate() + 1)
        } else {
            e = new Date(s.getTime() + (p.dur ?? 60) * 60000)
        }
        return { summary: name, start: s.getTime(), end: e.getTime(), allDay: false }
    }

    function open(ev) {
        Calendar.show(new Date(ev.start))
        Calendar.draft(ev.start, ev.allDay)
        Calendar.editing = Object.assign({}, Calendar.editing, { summary: ev.summary, start: ev.start, end: ev.end, allDay: ev.allDay })
    }

    function search(text, scoped) {
        const q = text.trim()
        const out = []

        const ev = q ? root.draft(q, scoped) : null
        if (ev) {
            const clash = Calendar.events.filter(e => !e.allDay && !ev.allDay && e.start < ev.end && e.end > ev.start)
            out.push({
                key: "cal:new",
                kind: "events",
                section: "Calendar",
                icon: "event",
                title: "Add “" + ev.summary + "”",
                subtitle: root.when(ev) + (clash.length ? " · overlaps " + clash[0].summary : ""),
                badge: "New event",
                score: 1.4,
                pinned: true,
                primary: "Add to calendar",
                activate: () => Calendar.save(Object.assign({ id: 0, location: "", notes: "" }, ev)),
                altActivate: () => root.open(ev),
                altHint: "Edit before adding",
                altIcon: "edit"
            })
        }

        if (scoped || /^(what'?s\s+on|agenda|upcoming|events?|schedule)\b/i.test(q)) {
            const now = Date.now()
            const filter = scoped && !ev ? q.toLowerCase() : ""
            const upcoming = Calendar.events
                .filter(e => e.end > now && (!filter || (e.summary ?? "").toLowerCase().includes(filter)))
                .sort((a, b) => a.start - b.start)
                .slice(0, 12)
            upcoming.forEach((e, i) => out.push({
                key: "cal:" + (e.id ?? e.start) + ":" + e.start,
                kind: "events",
                section: "Upcoming",
                icon: "event",
                title: e.summary,
                subtitle: root.when(e) + (e.location ? " · " + e.location : ""),
                dot: e.color ?? "",
                badge: e.source || "",
                score: 1 - i * 0.01,
                pinned: true,
                primary: "Open in calendar",
                activate: () => {
                    Calendar.show(new Date(e.start))
                    if (!e.readonly)
                        Calendar.edit(e)
                }
            }))
        }

        root.results = out
    }
}
