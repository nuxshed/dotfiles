import QtQuick
import Quickshell
import Quickshell.Io
import "../../../config"
import ".."

Provider {
    id: root

    name: "units"
    label: "Services"
    prefix: "u"
    icon: "dns"
    keywords: ["services", "systemd", "units", "daemons"]
    weight: 0.7
    cap: 2

    property var units: []
    property real loaded: 0
    property string last: ""
    property bool lastScoped: false

    function dot(u) {
        if (u.active === "failed")
            return Colors.red
        if (u.active === "active")
            return u.sub === "running" || u.sub === "waiting" ? Colors.green : Colors.cyan
        if (u.active === "activating" || u.active === "deactivating" || u.active === "reloading")
            return Colors.yellow
        return ""
    }

    function ctl(u, verb) {
        if (u.user)
            Quickshell.execDetached(["systemctl", "--user", verb, u.unit])
        else
            root.terminal(`sudo systemctl ${verb} ${root.q(u.unit)} && systemctl status --no-pager -n 5 ${root.q(u.unit)}; echo; read -r -p 'Press Enter to close' _`)
    }

    function q(s) {
        return "'" + s.replace(/'/g, "'\\''") + "'"
    }

    function clear() {
        root.last = ""
        root.lastScoped = false
        root.results = []
    }

    function terminal(script) {
        Quickshell.execDetached([SpotlightConfig.terminal, "-e", "sh", "-c", script])
    }

    function itemFor(u, score) {
        const scope = u.user ? "--user " : ""
        const running = u.active === "active"
        const actions = [
            running ? { icon: "stop", title: "Stop", run: () => root.ctl(u, "stop") } : { icon: "play_arrow", title: "Start", run: () => root.ctl(u, "start") },
            { icon: "info", title: "Status", run: () => root.terminal(`systemctl ${scope}status ${root.q(u.unit)} --no-pager; read -r -p 'Press Enter to close' _`) }
        ]
        return {
            key: "unit:" + (u.user ? "user:" : "system:") + u.unit,
            kind: "units",
            view: "unit",
            section: "Services",
            icon: u.unit.endsWith(".timer") ? "schedule" : "dns",
            title: u.unit.replace(/\.service$/, ""),
            subtitle: (u.description && u.description !== u.unit ? u.description + " · " : "") + (u.user ? "user" : "system"),
            badge: u.active === "active" ? u.sub : u.active,
            dot: root.dot(u),
            score: score,
            copy: u.unit,
            copyTitle: "Copy unit name",
            primary: "Restart",
            activate: () => root.ctl(u, "restart"),
            altActivate: () => root.terminal(`journalctl ${scope}-u ${root.q(u.unit)} -n 200 -f`),
            altHint: "Follow logs",
            altIcon: "subject",
            actions: actions,
            details: [
                { label: "State", value: u.load + " · " + u.active + " (" + u.sub + ")" },
                { label: "Scope", value: u.user ? "user" : "system" }
            ]
        }
    }

    function refresh() {
        if (fetch.running)
            return
        fetch.running = true
    }

    function search(text, scoped) {
        root.last = text
        root.lastScoped = !!scoped
        if (Date.now() - root.loaded > 5000)
            root.refresh()

        const q = text.trim().replace(/^(restart|start|stop|logs?)\s+/i, "")
        if (!scoped && q.length < 3) {
            root.results = []
            return
        }

        const out = []
        const ql = q.toLowerCase()
        for (const u of root.units) {
            if (!scoped && !u.lower.includes(ql))
                continue
            const name = u.unit.replace(/\.(service|timer)$/, "")
            const s = q ? Math.max(Fuzzy.score(q, name), Fuzzy.score(q, u.description ?? "") * 0.8) : (u.active === "failed" ? 0.6 : 0.4)
            if (s < (scoped ? 0 : 0.6))
                continue
            out.push(root.itemFor(u, s + (u.active === "failed" ? 0.05 : 0) + (u.user ? 0.02 : 0)))
        }
        out.sort((a, b) => b.score - a.score)
        root.results = out.slice(0, 60)
    }

    function accept(text) {
        const all = []
        for (const [i, chunk] of text.split("\n===\n").entries()) {
            try {
                for (const u of JSON.parse(chunk))
                    if (u.load !== "not-found")
                        all.push(Object.assign({ user: i === 0, lower: (u.unit + " " + (u.description ?? "")).toLowerCase() }, u))
            } catch (e) {}
        }
        root.units = all
        root.loaded = Date.now()
        if (root.last || root.lastScoped)
            root.search(root.last, root.lastScoped)
    }

    readonly property Process fetch: Process {
        command: ["sh", "-c", "systemctl --user list-units --all --type=service,timer --output=json --no-pager; printf '\\n===\\n'; systemctl list-units --all --type=service,timer --output=json --no-pager"]
        stdout: StdioCollector {
            onStreamFinished: root.accept(text)
        }
    }
}
