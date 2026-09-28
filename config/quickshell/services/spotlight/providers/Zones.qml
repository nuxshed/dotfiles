import QtQuick
import Quickshell
import Quickshell.Io
import ".."

Provider {
    id: root

    name: "zones"
    label: "Time zones"
    prefix: "tz"
    icon: "schedule"
    keywords: ["timezone", "time zones", "world clock", "time in"]
    weight: 1

    property var zones: ({})
    property var places: ({})
    property string local: "UTC"
    property string last: ""
    property bool lastScoped: false

    readonly property var abbreviations: ({
        ist: "Asia/Kolkata", pst: "America/Los_Angeles", pdt: "America/Los_Angeles", pt: "America/Los_Angeles", pacific: "America/Los_Angeles",
        mst: "America/Denver", mdt: "America/Denver", mt: "America/Denver", cst: "America/Chicago", cdt: "America/Chicago", ct: "America/Chicago",
        est: "America/New_York", edt: "America/New_York", et: "America/New_York", eastern: "America/New_York",
        gmt: "UTC", utc: "UTC", z: "UTC", zulu: "UTC", bst: "Europe/London", cet: "Europe/Paris", cest: "Europe/Paris",
        eet: "Europe/Athens", eest: "Europe/Athens", jst: "Asia/Tokyo", kst: "Asia/Seoul", sgt: "Asia/Singapore", hkt: "Asia/Hong_Kong",
        aest: "Australia/Sydney", aedt: "Australia/Sydney", nzst: "Pacific/Auckland", nzdt: "Pacific/Auckland", msk: "Europe/Moscow",
        pkt: "Asia/Karachi", wib: "Asia/Jakarta", gst: "Asia/Dubai", npt: "Asia/Kathmandu"
    })

    readonly property var aliases: ({
        "nyc": "America/New_York", "ny": "America/New_York", "boston": "America/New_York", "washington": "America/New_York", "dc": "America/New_York", "miami": "America/New_York", "atlanta": "America/New_York",
        "sf": "America/Los_Angeles", "san francisco": "America/Los_Angeles", "la": "America/Los_Angeles", "seattle": "America/Los_Angeles", "silicon valley": "America/Los_Angeles", "san jose": "America/Los_Angeles", "portland": "America/Los_Angeles",
        "austin": "America/Chicago", "dallas": "America/Chicago", "houston": "America/Chicago", "seattle wa": "America/Los_Angeles",
        "bangalore": "Asia/Kolkata", "bengaluru": "Asia/Kolkata", "mumbai": "Asia/Kolkata", "delhi": "Asia/Kolkata", "new delhi": "Asia/Kolkata", "hyderabad": "Asia/Kolkata", "chennai": "Asia/Kolkata", "pune": "Asia/Kolkata", "kolkata": "Asia/Kolkata", "calcutta": "Asia/Kolkata",
        "beijing": "Asia/Shanghai", "shenzhen": "Asia/Shanghai", "osaka": "Asia/Tokyo", "kyoto": "Asia/Tokyo", "abu dhabi": "Asia/Dubai",
        "munich": "Europe/Berlin", "frankfurt": "Europe/Berlin", "hamburg": "Europe/Berlin", "barcelona": "Europe/Madrid", "milan": "Europe/Rome", "zurich": "Europe/Zurich", "geneva": "Europe/Zurich",
        "manchester": "Europe/London", "edinburgh": "Europe/London", "st petersburg": "Europe/Moscow", "melbourne": "Australia/Melbourne", "perth": "Australia/Perth",
        "usa": "America/New_York", "us": "America/New_York", "america": "America/New_York", "united states": "America/New_York",
        "uk": "Europe/London", "england": "Europe/London", "britain": "Europe/London", "great britain": "Europe/London", "united kingdom": "Europe/London",
        "canada": "America/Toronto", "australia": "Australia/Sydney", "russia": "Europe/Moscow", "brazil": "America/Sao_Paulo", "mexico": "America/Mexico_City",
        "china": "Asia/Shanghai", "indonesia": "Asia/Jakarta", "korea": "Asia/Seoul", "south korea": "Asia/Seoul", "uae": "Asia/Dubai", "emirates": "Asia/Dubai",
        "germany": "Europe/Berlin", "spain": "Europe/Madrid", "portugal": "Europe/Lisbon", "new zealand": "Pacific/Auckland", "nz": "Pacific/Auckland"
    })

    function flag(cc) {
        if (!cc || cc.length !== 2)
            return ""
        return String.fromCodePoint(0x1F1E6 + cc.charCodeAt(0) - 65, 0x1F1E6 + cc.charCodeAt(1) - 65)
    }

    function load(text) {
        const zones = {}
        const countries = {}
        let local = "UTC"
        for (const line of text.split("\n")) {
            const f = line.split("\t")
            if (f[0] === "Z" && f.length >= 4) {
                const [sign, h, m] = [f[3][0] === "-" ? -1 : 1, parseInt(f[3].substr(1, 2)), parseInt(f[3].substr(3, 2))]
                zones[f[2]] = { tz: f[2], cc: f[1], off: sign * (h * 60 + m), abbr: /^[A-Z]+$/.test(f[4] ?? "") ? f[4] : "UTC" + (sign < 0 ? "−" : "+") + h + (m ? ":" + String(m).padStart(2, "0") : "") }
            } else if (f[0] === "C" && f.length >= 3) {
                countries[f[1]] = f[2]
            } else if (f[0] === "L" && f[1]) {
                local = f[1]
            }
        }
        if (zones.UTC)
            zones.UTC.abbr = "UTC"

        const places = {}
        const add = (k, tz) => {
            if (zones[tz] && !(k in places))
                places[k] = tz
        }
        for (const tz in zones) {
            if (tz.split("/").length === 2)
                add(tz.split("/").pop().replace(/_/g, " ").toLowerCase(), tz)
            add(tz.toLowerCase(), tz)
        }
        for (const k in root.aliases)
            places[k] = root.aliases[k]
        for (const tz in zones) {
            const name = (countries[zones[tz].cc] ?? "").toLowerCase()
            if (name)
                add(name.replace(/\s*\(.*\)/, ""), tz)
        }

        for (const tz in zones)
            zones[tz].country = countries[zones[tz].cc] ?? ""

        root.zones = zones
        root.places = places
        root.local = zones[local] ? local : "UTC"
        if (root.last)
            root.search(root.last, root.lastScoped)
    }

    function resolve(text, allowAbbr) {
        const s = text.trim().toLowerCase().replace(/^the\s+/, "").replace(/[?.!]+$/, "")
        if (!s)
            return null
        if (["here", "local", "my time", "me"].includes(s))
            return root.zones[root.local] ?? null
        if (allowAbbr && root.abbreviations[s])
            return root.zones[root.abbreviations[s]] ?? null
        const tz = root.places[s] ?? root.places[s.replace(/\s+/g, "_")]
        return tz ? root.zones[tz] ?? null : null
    }

    function parseTime(text) {
        const s = text.trim().toLowerCase()
        if (s === "noon")
            return { h: 12, m: 0, explicit: true }
        if (s === "midnight")
            return { h: 0, m: 0, explicit: true }
        const m = s.match(/^(\d{1,2})(?:[:.](\d{2}))?\s*(am|pm|a\.m\.|p\.m\.)?$/)
        if (!m)
            return null
        let h = parseInt(m[1])
        const min = m[2] ? parseInt(m[2]) : 0
        const mer = m[3] ? m[3][0] : ""
        if (min > 59 || h > 23 || (mer && (h < 1 || h > 12)))
            return null
        if (mer === "p" && h < 12)
            h += 12
        if (mer === "a" && h === 12)
            h = 0
        return { h: h, m: min, explicit: !!(m[2] || mer) }
    }

    function wall(ms, zone) {
        const d = new Date(ms + zone.off * 60000)
        return { y: d.getUTCFullYear(), mo: d.getUTCMonth(), d: d.getUTCDate(), h: d.getUTCHours(), m: d.getUTCMinutes() }
    }

    function clock(w) {
        return String(w.h).padStart(2, "0") + ":" + String(w.m).padStart(2, "0")
    }

    function gap(minutes) {
        const a = Math.abs(minutes)
        const h = Math.floor(a / 60)
        const m = a % 60
        return (h ? h + "h" : "") + (h && m ? " " : "") + (m ? m + "m" : "") || "0h"
    }

    function city(zone) {
        return zone.tz === "UTC" ? "Coordinated Universal Time" : zone.tz.split("/").pop().replace(/_/g, " ") + (zone.country ? ", " + zone.country.replace(/\s*\(.*\)/, "") : "")
    }

    function side(zone, w) {
        return { flag: root.flag(zone.cc), code: zone.abbr, name: root.city(zone), amount: root.clock(w) }
    }

    function pair(src, dst, time, score) {
        const now = Date.now()
        const today = root.wall(now, src)
        const utc = time ? Date.UTC(today.y, today.mo, today.d, time.h, time.m) - src.off * 60000 : now
        const a = root.wall(utc, src)
        const b = root.wall(utc, dst)
        const days = Math.round((Date.UTC(b.y, b.mo, b.d) - Date.UTC(a.y, a.mo, a.d)) / 86400000)
        const diff = dst.off - src.off
        const dstName = dst.tz.split("/").pop().replace(/_/g, " ")
        const rel = diff === 0 ? "same time as " + src.abbr : root.gap(diff) + (diff > 0 ? " ahead of " : " behind ") + src.abbr
        const day = days === 1 ? " · next day" : days === -1 ? " · previous day" : ""
        const text = `${root.clock(a)} ${src.abbr} = ${root.clock(b)} ${dst.abbr}${days === 1 ? " (+1 day)" : days === -1 ? " (−1 day)" : ""}`

        return {
            key: "tz:" + src.tz + ":" + dst.tz,
            kind: "zones",
            view: "pair",
            section: "Time zones",
            icon: "schedule",
            title: text,
            subtitle: dstName + " is " + rel + day,
            from: root.side(src, a),
            to: root.side(dst, b),
            score: score,
            pinned: true,
            primary: "Copy " + root.clock(b) + " " + dst.abbr,
            activate: () => Quickshell.execDetached(["wl-copy", "--", root.clock(b) + " " + dst.abbr]),
            altActivate: () => Quickshell.execDetached(["wl-copy", "--", text]),
            altHint: "Copy conversion",
            altIcon: "content_copy",
            details: [
                { label: "From", value: src.tz + " (UTC" + (src.off < 0 ? "−" : "+") + root.gap(src.off) + ")" },
                { label: "To", value: dst.tz + " (UTC" + (dst.off < 0 ? "−" : "+") + root.gap(dst.off) + ")" }
            ]
        }
    }

    function parse(text, scoped) {
        const here = root.zones[root.local]
        const q = text.trim().toLowerCase().replace(/[?]+$/, "")
        if (!here || !q)
            return []

        const now = q.match(/^(?:what(?:'s| is)?\s+the\s+time\s+in|what\s+time\s+is\s+it\s+in|current\s+time\s+in|time\s+(?:in|at)|now\s+in|clock\s+in)\s+(.+)$/) ?? q.match(/^(.+?)\s+(?:time|clock)$/)
        if (now) {
            const z = root.resolve(now[1], true)
            return z ? [root.pair(here, z, null, 1.5)] : []
        }

        const conv = q.match(/^(.+?)\s+(?:to|in|into|->|→|=)\s+(.+)$/)
        if (conv) {
            const dst = root.resolve(conv[2], true)
            if (!dst)
                return []
            const left = conv[1].match(/^(noon|midnight|\d{1,2}(?:[:.]\d{2})?\s*(?:am|pm|a\.m\.|p\.m\.)?)(?:\s+(?:in\s+|at\s+)?(.+))?$/)
            if (left) {
                const t = root.parseTime(left[1])
                if (t && (t.explicit || scoped)) {
                    const src = left[2] ? root.resolve(left[2], true) : here
                    if (src)
                        return [root.pair(src, dst, t, 1.55)]
                }
            }
            const src = root.resolve(conv[1], true)
            return src ? [root.pair(src, dst, null, 1.5)] : []
        }

        const at = q.match(/^(noon|midnight|\d{1,2}(?:[:.]\d{2})?\s*(?:am|pm|a\.m\.|p\.m\.)?)\s+(?:in\s+|at\s+)?(.+)$/)
        if (at) {
            const t = root.parseTime(at[1])
            const src = root.resolve(at[2], true)
            if (t && src && (t.explicit || scoped))
                return src.tz === here.tz ? [] : [root.pair(src, here, t, 1.5)]
        }

        const bare = root.resolve(q, scoped)
        if (bare && (scoped || q.length >= 4))
            return [root.pair(here, bare, null, scoped ? 1.5 : 0.62)]
        return []
    }

    function search(text, scoped) {
        root.last = text
        root.lastScoped = !!scoped
        root.results = root.parse(text, !!scoped)
    }

    function clear() {
        root.last = ""
        root.results = []
    }

    readonly property Process loader: Process {
        running: true
        command: ["sh", "-c", `
d=/etc/zoneinfo; [ -f "$d/zone.tab" ] || d=/usr/share/zoneinfo
awk -F'\\t' '!/^#/ && NF >= 3 {print $1 "\\t" $3}' "$d/zone.tab" | while IFS="$(printf '\\t')" read -r cc z; do
  set -- $(TZ=":$d/$z" date +'%z %Z')
  printf 'Z\\t%s\\t%s\\t%s\\t%s\\n' "$cc" "$z" "$1" "$2"
done
printf 'Z\\t\\tUTC\\t+0000\\tUTC\\n'
awk -F'\\t' '!/^#/ && NF >= 2 {print "C\\t" $1 "\\t" $2}' "$d/iso3166.tab"
printf 'L\\t%s\\n' "$(readlink -f /etc/localtime | sed 's#.*/zoneinfo/##')"
`]
        stdout: StdioCollector {
            onStreamFinished: root.load(text)
        }
    }

    readonly property Timer refresh: Timer {
        interval: 30 * 60 * 1000
        repeat: true
        running: true
        onTriggered: root.loader.running = true
    }
}
