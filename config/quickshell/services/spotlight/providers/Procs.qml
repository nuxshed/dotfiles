import QtQuick
import "../../../services"
import ".."

Provider {
    id: root

    name: "procs"
    label: "Processes"
    prefix: "k"
    weight: 0.9
    cap: 6

    readonly property var tabs: [
        { id: "overview", title: "System Monitor", icon: "timeline", keywords: ["monitor", "sysmon", "task manager", "activity"] },
        { id: "cpu", title: "System Monitor — CPU", icon: "memory", keywords: ["cpu", "usage", "cores"] },
        { id: "gpu", title: "System Monitor — GPU", icon: "videogame_asset", keywords: ["gpu", "nvidia", "vram"] },
        { id: "memory", title: "System Monitor — Memory", icon: "storage", keywords: ["memory", "ram", "swap"] },
        { id: "network", title: "System Monitor — Network", icon: "wifi", keywords: ["network", "bandwidth", "wifi"] },
        { id: "disk", title: "System Monitor — Disk", icon: "album", keywords: ["disk", "io"] },
        { id: "battery", title: "System Monitor — Battery", icon: "battery_full", keywords: ["battery", "power", "charge"] },
        { id: "storage", title: "System Monitor — Storage", icon: "sd_storage", keywords: ["storage", "disk space", "treemap", "du"] },
        { id: "sensors", title: "System Monitor — Sensors", icon: "whatshot", keywords: ["sensors", "temperature", "fans", "fan curve"] },
        { id: "processes", title: "System Monitor — Processes", icon: "view_list", keywords: ["processes", "tasks", "kill", "top"] }
    ]

    function search(text) {
        const out = []
        const q = text.trim().toLowerCase()

        for (const t of root.tabs) {
            const s = Fuzzy.best(text, [t.title].concat(t.keywords))
            if (s < 0)
                continue
            out.push({
                key: "sysmon:" + t.id,
                kind: "commands",
                section: "Commands",
                icon: t.icon,
                title: t.title,
                subtitle: "Open",
                score: s * 0.9,
                activate: () => SysMon.show(t.id)
            })
        }

        const m = q.match(/^(?:kill|end)\s+(.+)$/)
        const needle = m ? m[1] : (text.startsWith("k ") ? q : "")
        if (needle.length > 0) {
            if (Date.now() - SysMon.lastProcTime > 5000)
                SysMon.refreshProcs()
            const groups = {}
            for (const p of SysMon.procs) {
                if (p.mem === 0 || !p.name.toLowerCase().includes(needle))
                    continue
                const g = groups[p.name] ?? (groups[p.name] = { name: p.name, pids: [], cpu: 0, mem: 0 })
                g.pids.push(p.pid)
                g.cpu += p.cpu
                g.mem += p.mem
            }
            for (const g of Object.values(groups).sort((a, b) => b.cpu - a.cpu).slice(0, 5)) {
                out.push({
                    key: "kill:" + g.name,
                    kind: "procs",
                    section: "Processes",
                    icon: "cancel",
                    title: `End ${g.name}`,
                    subtitle: `${g.pids.length} process${g.pids.length === 1 ? "" : "es"} · ${g.cpu.toFixed(1)}% CPU · ${SysMon.fmtBytes(g.mem, 0)}`,
                    score: 1,
                    activate: () => SysMon.endProcesses(g.pids, false),
                    altActivate: () => SysMon.endProcesses(g.pids, true),
                    altHint: "Force kill"
                })
            }
        }

        root.results = out
    }
}
