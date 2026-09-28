import QtQuick
import Quickshell
import Quickshell.Io
import "../../../config"
import ".."

Provider {
    id: root

    name: "calc"
    label: "Calculator"
    prefix: "="
    weight: 1

    property string pending: ""
    property var fx: null
    property bool explicit: false

    readonly property var operators: /[+\-*/^%]|\bto\b|\bin\b|\bas\b/
    readonly property var incomplete: /[+\-*/^%(,.]\s*$/
    readonly property var keywordTargets: /^([+-]|hex|bin|oct|dec|duo|base|roman|bases|fraction|frac|sci|utc|time|unicode|calendars|factors|optimal|prefix|mixed|partial)/i

    readonly property var currencies: ({
        USD: ["US Dollar", "$", "US"], EUR: ["Euro", "€", "EU"], GBP: ["British Pound", "£", "GB"],
        INR: ["Indian Rupee", "₹", "IN"], JPY: ["Japanese Yen", "¥", "JP"], CNY: ["Chinese Yuan", "CN¥", "CN"],
        AUD: ["Australian Dollar", "A$", "AU"], CAD: ["Canadian Dollar", "C$", "CA"], CHF: ["Swiss Franc", "CHF ", "CH"],
        SGD: ["Singapore Dollar", "S$", "SG"], HKD: ["Hong Kong Dollar", "HK$", "HK"], NZD: ["New Zealand Dollar", "NZ$", "NZ"],
        SEK: ["Swedish Krona", "kr ", "SE"], NOK: ["Norwegian Krone", "kr ", "NO"], DKK: ["Danish Krone", "kr ", "DK"],
        KRW: ["South Korean Won", "₩", "KR"], AED: ["UAE Dirham", "AED ", "AE"], SAR: ["Saudi Riyal", "SAR ", "SA"],
        RUB: ["Russian Ruble", "₽", "RU"], BRL: ["Brazilian Real", "R$", "BR"], MXN: ["Mexican Peso", "MX$", "MX"],
        ZAR: ["South African Rand", "R ", "ZA"], TRY: ["Turkish Lira", "₺", "TR"], THB: ["Thai Baht", "฿", "TH"],
        IDR: ["Indonesian Rupiah", "Rp ", "ID"], MYR: ["Malaysian Ringgit", "RM ", "MY"], PHP: ["Philippine Peso", "₱", "PH"],
        PLN: ["Polish Złoty", "zł ", "PL"], CZK: ["Czech Koruna", "Kč ", "CZ"], ILS: ["Israeli Shekel", "₪", "IL"],
        NPR: ["Nepalese Rupee", "Rs ", "NP"], LKR: ["Sri Lankan Rupee", "Rs ", "LK"], PKR: ["Pakistani Rupee", "Rs ", "PK"],
        BDT: ["Bangladeshi Taka", "৳", "BD"], VND: ["Vietnamese Dong", "₫", "VN"], BTC: ["Bitcoin", "₿", ""]
    })

    readonly property var words: [
        [/\b(us\s*)?dollars?\b|\bbucks\b/gi, "USD"], [/\brupees?\b|\brs\.?(?=\s|\d|$)/gi, "INR"], [/\beuros?\b/gi, "EUR"],
        [/\byen\b/gi, "JPY"], [/\bpounds?(\s+sterling)?\b|\bquid\b/gi, "GBP"], [/\byuan\b|\brmb\b/gi, "CNY"],
        [/\bfrancs?\b/gi, "CHF"], [/\bbitcoins?\b/gi, "BTC"], [/\bwon\b/gi, "KRW"], [/\bdirhams?\b/gi, "AED"],
        [/\bdong\b/gi, "VND"], [/\bbaht\b/gi, "THB"], [/\bringgits?\b/gi, "MYR"], [/\brupiahs?\b/gi, "IDR"],
        [/\bliras?\b/gi, "TRY"], [/\bzlot(y|ys|ych)\b/gi, "PLN"], [/\btakas?\b/gi, "BDT"], [/\bshekels?\b/gi, "ILS"],
        [/\broubles?\b|\brubles?\b/gi, "RUB"],
        [/\$/g, " USD "], [/€/g, " EUR "], [/£/g, " GBP "], [/¥/g, " JPY "], [/₹/g, " INR "], [/₩/g, " KRW "], [/₿/g, " BTC "],
        [/₫/g, " VND "], [/฿/g, " THB "], [/₺/g, " TRY "], [/₽/g, " RUB "], [/₪/g, " ILS "]
    ]

    readonly property var multipliers: ({
        K: 1e3, THOUSAND: 1e3, M: 1e6, MIL: 1e6, MILL: 1e6, MN: 1e6, MILLION: 1e6, MILLIONS: 1e6,
        B: 1e9, BN: 1e9, BIL: 1e9, BILLION: 1e9, BILLIONS: 1e9,
        LAKH: 1e5, LAKHS: 1e5, LAC: 1e5, LACS: 1e5, CR: 1e7, CRORE: 1e7, CRORES: 1e7
    })

    function flag(cc) {
        if (!cc)
            return ""
        return String.fromCodePoint(0x1F1E6 + cc.charCodeAt(0) - 65, 0x1F1E6 + cc.charCodeAt(1) - 65)
    }

    function currency(code) {
        const c = root.currencies[code]
        return { code: code, name: c[0], symbol: c[1], flag: root.flag(c[2]) }
    }

    function money(n) {
        if (n !== 0 && Math.abs(n) < 0.01)
            return Number(n.toPrecision(3)).toString()
        return n.toLocaleString(Qt.locale("en_US"), "f", Number.isInteger(n) ? 0 : 2)
    }

    function rate(n) {
        if (n >= 100)
            return n.toLocaleString(Qt.locale("en_US"), "f", 2)
        if (n >= 1)
            return n.toFixed(4)
        return Number(n.toPrecision(4)).toString()
    }

    function parseCurrency(text) {
        let s = text
        for (const [re, code] of root.words)
            s = s.replace(re, " " + code + " ")
        s = s.toUpperCase().replace(/\s+/g, " ").trim()

        const mults = Object.keys(root.multipliers).sort((x, y) => y.length - x.length).join("|")
        const code = "([A-Z]{3})"
        const amount = `(\\d(?:[\\d.,_]|\\s(?=\\d))*)\\s*(?:(${mults})\\b)?`
        const m = s.match(new RegExp(`^(?:${amount}\\s*${code}|${code}\\s*${amount}|${code})(?:\\s+(?:TO|IN|INTO|AS)\\s+([A-Z]{3}(?:\\s*(?:,|AND)?\\s*[A-Z]{3})*))?$`))
        if (!m)
            return null

        const from = m[3] ?? m[4] ?? m[7]
        const raw = m[1] ?? m[5]
        const mult = root.multipliers[m[2] ?? m[6]] ?? 1
        const to = m[8] ? m[8].split(/\s*(?:,|AND|\s)\s*/).filter(c => c.length === 3) : []

        if (!root.currencies[from] || to.some(c => !root.currencies[c]))
            return null
        if (!raw && to.length === 0)
            return null

        let targets = to
        if (targets.length === 0) {
            const pool = [SpotlightConfig.homeCurrency].concat(SpotlightConfig.currencies)
            targets = pool.filter((c, i) => c !== from && pool.indexOf(c) === i).slice(0, 3)
        }

        return {
            from: from,
            amount: raw ? parseFloat(raw.replace(/[\s,_]/g, "")) * mult : 1,
            to: targets.filter(c => c !== from)
        }
    }

    function normalise(text) {
        let q = text.replace(/\bin\b/gi, "to")

        const temp = q.match(/^\s*(-?[\d.]+)\s*°?\s*([fck])\s+to\s+°?\s*([fck])\s*$/i)
        if (temp) {
            const u = c => ({ f: "°F", c: "°C", k: "K" })[c.toLowerCase()]
            return `${temp[1]} ${u(temp[2])} to -${u(temp[3])}`
        }

        q = q.replace(/\b(\d*\.?\d*)\s*(kib|mib|gib|tib|kb|mb|gb|tb|pb)\b/gi, (m, n, u) => {
            const up = u.length === 3 && u.charAt(1).toLowerCase() === "i"
                ? u.charAt(0).toUpperCase() + "iB"
                : u.charAt(0).toUpperCase() + "B"
            return n + " " + up
        })

        const conv = q.match(/^(.*[a-z].*)\bto\s+(\S.*)$/i)
        if (conv && !root.keywordTargets.test(conv[2]))
            q = conv[1] + "to -" + conv[2]
        return q
    }

    function looksNumeric(text) {
        if (!/\d/.test(text))
            return false
        if (/\b\d{1,2}(:\d{2})?\s*(am|pm)\b|\b\d{1,2}:\d{2}\b/i.test(text))
            return false
        if ((text.match(/[a-z]{2,}/gi) ?? []).filter(w => !/^(to|in|as|of)$/i.test(w)).length >= 3)
            return false
        if (root.incomplete.test(text))
            return false
        return root.operators.test(text) || /^\s*[\d.]+\s*[a-z%°]+\s*$/i.test(text)
    }

    function reset() {
        root.pending = ""
        root.fx = null
        root.results = []
        proc.running = false
    }

    function search(text) {
        let q = text.trim()
        const explicit = q.startsWith("=")
        if (explicit)
            q = q.substring(1).trim()

        if (!q)
            return root.reset()

        const fx = root.parseCurrency(q)
        if (fx && fx.to.length > 0) {
            root.pending = q
            root.fx = fx
            proc.running = false
            proc.command = ["sh", "-c", 'printf "%s\\n" "$@" | qalc -t', "sh"].concat(fx.to.map(c => `1 ${fx.from} to -${c}`))
            proc.running = true
            return
        }

        if (!explicit && !root.looksNumeric(q))
            return root.reset()

        root.pending = q
        root.explicit = explicit
        root.fx = null
        proc.running = false
        proc.command = ["qalc", "-t", "-m", "500", "-set", "precision 7", root.normalise(q)]
        proc.running = true
    }

    function acceptFx(output) {
        const fx = root.fx
        const lines = output.replace(/\x1b\[[0-9;]*m/g, "").split("\n").map(l => l.trim()).filter(l => l.length > 0 && !l.startsWith(">"))
        const out = []

        fx.to.forEach((code, i) => {
            const m = (lines[i] ?? "").replace(/,/g, "").match(/-?\d+(?:\.\d+)?(?:E[-+]?\d+)?/i)
            if (!m)
                return
            const r = parseFloat(m[0])
            const value = fx.amount * r
            const from = root.currency(fx.from)
            const to = root.currency(code)
            const plain = value.toFixed(value !== 0 && Math.abs(value) < 0.01 ? 6 : 2)

            out.push({
                key: "fx:" + fx.from + ":" + code,
                kind: "calc",
                view: "pair",
                section: "Currency",
                icon: "attach_money",
                title: to.symbol + root.money(value),
                subtitle: `1 ${fx.from} = ${root.rate(r)} ${code}`,
                from: { flag: from.flag, code: from.code, name: from.name, amount: from.symbol + root.money(fx.amount) },
                to: { flag: to.flag, code: to.code, name: to.name, amount: to.symbol + root.money(value) },
                score: 1.6 - i * 0.01,
                pinned: true,
                primary: "Copy " + plain,
                activate: () => Quickshell.execDetached(["wl-copy", "--", plain]),
                altActivate: () => Quickshell.execDetached(["wl-copy", "--", to.symbol + root.money(value)]),
                altHint: "Copy with symbol",
                altIcon: "content_copy",
                details: [
                    { label: "Rate", value: `1 ${fx.from} = ${root.rate(r)} ${code}` },
                    { label: "Inverse", value: `1 ${code} = ${root.rate(1 / r)} ${fx.from}` }
                ]
            })
        })

        root.results = out
    }

    function accept(output) {
        if (root.fx)
            return root.acceptFx(output)

        const value = output.trim()
        if (!value || value === root.pending || /^error/i.test(value) || (!root.explicit && /·/.test(value) && !/[*·]/.test(root.pending))) {
            root.results = []
            return
        }

        const expr = root.pending
        root.results = [{
            key: "calc",
            kind: "calc",
            view: "calc",
            section: "Calculator",
            icon: "functions",
            title: value,
            subtitle: expr,
            score: 1.6,
            pinned: true,
            primary: "Copy result",
            activate: () => Quickshell.execDetached(["wl-copy", "--", value]),
            altActivate: () => Quickshell.execDetached(["wl-copy", "--", expr + " = " + value]),
            altHint: "Copy equation",
            altIcon: "content_copy",
            details: [{ label: "Evaluated", value: root.normalise(expr) }]
        }]
    }

    readonly property Process proc: Process {
        stdout: StdioCollector {
            onStreamFinished: root.accept(text)
        }
        onExited: (code) => {
            if (code !== 0)
                root.results = []
        }
    }
}
