pragma Singleton
pragma ComponentBehavior: Bound

import QtQuick
import Quickshell
import Quickshell.Io

Singleton {
    id: root

    readonly property string script: Quickshell.env("HOME") + "/dotfiles/bin/qs-sysmon"
    readonly property int histLen: 90

    property bool open: false
    readonly property string hostname: hostFile.text().trim()
    property string tab: "performance"
    property string perfTab: "overview"
    property int sensorsTab: 0

    property real cpu: 0
    property var cores: []
    property var cpuHist: []
    property int cpuCount: 1
    property int freq: 0
    property string cpuModel: ""
    property real cpuTemp: 0
    property var cpuTempHist: []
    property var gpuTempHist: []
    property string load: ""
    property real uptime: 0

    property real memTotal: 0
    property real memAvail: 0
    property real memCached: 0
    property real swapTotal: 0
    property real swapFree: 0
    readonly property real memUsed: memTotal - memAvail
    readonly property real memPct: memTotal > 0 ? memUsed / memTotal * 100 : 0
    property var memHist: []

    property real netRx: 0
    property real netTx: 0
    property string netIface: ""
    property real netRxTotal: 0
    property real netTxTotal: 0
    property var netRxHist: []
    property var netTxHist: []

    property real diskRead: 0
    property real diskWrite: 0
    property real diskReadTotal: 0
    property real diskWriteTotal: 0
    property var diskReadHist: []
    property var diskWriteHist: []
    property real rootUsed: 0
    property real rootSize: 0

    property string gpuState: "none"
    property string gpuName: ""
    property real gpuUtil: 0
    property real gpuTemp: 0
    property real gpuMemUsed: 0
    property real gpuMemTotal: 0
    property real gpuPower: 0
    property var gpuHist: []
    property int igpuFreq: 0
    property int igpuMax: 0

    property bool batPresent: false
    property string batStatus: ""
    property int batCapacity: 0
    property real batEnergyNow: 0
    property real batEnergyFull: 0
    property real batEnergyDesign: 0
    property real batPower: 0
    property real batVoltage: 0
    property int batCycles: 0
    property int batThreshold: 0
    property string batModel: ""
    property string batVendor: ""
    property string batTech: ""
    property bool acOnline: false
    property var batPowerHist: []
    property var batCapHist: []
    property var batApps: ({})
    property real batSince: 0
    property var batSession: []
    property real lastProcTime: 0
    property real lastBatSave: 0
    readonly property real batAppTotal: Object.values(batApps).reduce((a, e) => a + e.wh, 0)
    readonly property real batAppCpu: Object.values(batApps).reduce((a, e) => a + e.cpu, 0)
    readonly property var batAppList: Object.keys(batApps).map(n => ({ name: n, wh: batApps[n].wh, cpu: batApps[n].cpu })).sort((a, b) => batOnBattery ? b.wh - a.wh : b.cpu - a.cpu)
    readonly property real batHealth: batEnergyDesign > 0 ? batEnergyFull / batEnergyDesign * 100 : 0
    readonly property real batSmoothPower: {
        const h = batPowerHist.slice(-30).filter(v => v > 0);
        return h.length > 0 ? h.reduce((a, b) => a + b, 0) / h.length : 0;
    }
    readonly property real batTimeLeft: {
        if (batSmoothPower <= 0)
            return 0;
        if (batStatus === "Discharging")
            return batEnergyNow / batSmoothPower;
        if (batStatus === "Charging")
            return (batEnergyFull - batEnergyNow) / batSmoothPower;
        return 0;
    }

    property var temps: []
    property var fans: []
    property var curves: []
    property string curveProfile: ""
    property var profileCurves: []
    property bool curveBusy: false
    property bool curvePending: false
    property var gpuProcs: []
    property bool duFiles: false
    property var alertState: ({})

    property var info: ({})
    property var displays: []
    property var gpus: []

    property real trashSize: 0
    property int trashCount: 0
    property int nixGenerations: 0
    property real nixClosure: 0
    property bool maintenanceBusy: false
    property bool maintOpen: false
    property var generations: []

    property var filesystems: []
    property string duPath: ""
    property var duEntries: []
    property real duTotal: 0
    property bool scanning: false
    property string pendingScan: ""
    property string scanRoot: ""
    property var duCache: ({})
    property real duScanned: 0
    readonly property int duDepth: 4
    readonly property string cacheDir: Quickshell.env("HOME") + "/.cache/qs-sysmon"

    property int procCount: 0
    property int threadCount: 0
    property var procs: []
    property int selectedPid: 0

    property var prevCpu: null
    property var prevCores: null
    property var prevNet: null
    property var prevDisk: null
    property var prevProcs: ({})
    property real prevProcTotal: 0
    property real lastSample: 0

    function toggle(): void {
        if (!root.open) {
            root.tab = "performance";
            root.perfTab = "overview";
        }
        root.open = !root.open;
    }

    function show(tab: string): void {
        if (tab === "about" || tab === "fetch") {
            root.tab = "about";
        } else if (tab === "fans") {
            root.tab = "sensors";
            root.sensorsTab = 1;
        } else if (["overview", "cpu", "gpu", "memory", "network", "disk"].includes(tab)) {
            root.tab = "performance";
            root.perfTab = tab;
        } else if (tab.length > 0) {
            root.tab = tab;
        }
        root.open = true;
    }

    function push(hist: var, v: real): var {
        const h = hist.concat([v]);
        return h.length > root.histLen ? h.slice(h.length - root.histLen) : h;
    }

    function fmtBytes(b: real, dec: int): string {
        if (b < 1024)
            return Math.round(b) + " B";
        const u = ["KB", "MB", "GB", "TB"];
        let i = -1;
        do { b /= 1024; i++; } while (b >= 1024 && i < u.length - 1);
        return b.toFixed(dec === undefined ? 1 : dec) + " " + u[i];
    }

    function fmtRate(b: real): string {
        return root.fmtBytes(b, 1) + "/s";
    }

    function fmtDuration(s: real): string {
        s = Math.max(0, Math.floor(s));
        const d = Math.floor(s / 86400);
        const h = Math.floor(s % 86400 / 3600);
        const m = Math.floor(s % 3600 / 60);
        if (d > 0)
            return `${d}d ${h}h ${m}m`;
        if (h > 0)
            return `${h}h ${m}m`;
        return `${m}m`;
    }

    function fmtHours(h: real): string {
        if (h <= 0 || h > 99)
            return "—";
        const m = Math.round(h * 60);
        return m < 60 ? `${m}m` : `${Math.floor(m / 60)}h ${m % 60}m`;
    }

    function normPath(path: string): string {
        if (path.length === 0)
            path = Quickshell.env("HOME");
        if (path.length > 1 && path.endsWith("/"))
            path = path.slice(0, -1);
        return path;
    }

    function showFiles(path: string): void {
        path = root.normPath(path);
        root.duPath = path;
        root.duFiles = true;
        root.duEntries = [];
        root.scanning = true;
        filesProc.command = [root.script, "files", path];
        filesProc.running = true;
    }

    function parseFiles(text: string): void {
        const list = [];
        let total = 0;
        for (const line of text.split("\n")) {
            const i = line.indexOf("\t");
            if (i < 0)
                continue;
            const size = Number(line.slice(0, i));
            const path = line.slice(i + 1);
            total += size;
            list.push({ name: path.slice(path.lastIndexOf("/") + 1), path, size, dir: false, file: true });
        }
        list.sort((a, b) => b.size - a.size);
        const top = list.slice(0, 60);
        const rest = list.slice(60).reduce((a, e) => a + e.size, 0);
        if (rest > 0)
            top.push({ name: `${list.length - 60} more files`, path: root.duPath, size: rest, dir: false, file: false });
        root.duEntries = top;
        root.duTotal = total;
        root.duScanned = Date.now();
        root.scanning = false;
    }

    function remove(path: string, permanent: bool): void {
        rmProc.command = [root.script, permanent ? "rm" : "trash", path];
        rmProc.running = true;
    }

    function showDir(path: string): void {
        path = root.normPath(path);
        root.duPath = path;
        root.duFiles = false;
        const hit = root.duCache[path];
        if (hit) {
            root.duEntries = hit.entries;
            root.duTotal = hit.total;
            root.duScanned = hit.time;
            return;
        }
        root.scanDir(path);
    }

    function scanDir(path: string): void {
        path = root.normPath(path);
        root.duPath = path;
        root.duEntries = [];
        root.duTotal = 0;
        root.scanning = true;
        if (duProc.running) {
            root.pendingScan = path;
            return;
        }
        root.scanRoot = path;
        duProc.command = [root.script, "du", path];
        duProc.running = true;
    }

    function parseDu(text: string): void {
        if (root.pendingScan.length > 0)
            return;
        const base = root.scanRoot;
        const depth = p => p.split("/").length;
        const baseDepth = depth(base);
        const sizes = {};
        const children = {};
        for (const line of text.split("\n")) {
            const i = line.indexOf("\t");
            if (i < 0)
                continue;
            const size = Number(line.slice(0, i));
            const path = line.slice(i + 1);
            sizes[path] = size;
            if (path === base)
                continue;
            const parent = path.slice(0, path.lastIndexOf("/")) || "/";
            if (!children[parent])
                children[parent] = [];
            children[parent].push({ name: path.slice(path.lastIndexOf("/") + 1), path, size, dir: true });
        }
        const now = Date.now();
        const cache = Object.assign({}, root.duCache);
        for (const dir in sizes) {
            if (depth(dir) - baseDepth >= root.duDepth)
                continue;
            const list = children[dir] ?? [];
            const rest = sizes[dir] - list.reduce((a, e) => a + e.size, 0);
            if (rest > 0 && list.length > 0)
                list.push({ name: "Files", path: dir, size: rest, dir: false });
            list.sort((a, b) => b.size - a.size);
            cache[dir] = { entries: list, total: sizes[dir], time: now };
        }
        root.duCache = cache;
        root.scanning = false;
        root.showDir(root.duPath);
        root.saveDuCache();
    }

    function saveDuCache(): void {
        Quickshell.execDetached(["mkdir", "-p", root.cacheDir]);
        duCacheFile.setText(JSON.stringify(root.duCache));
    }

    function loadDuCache(): void {
        try {
            const t = duCacheFile.text();
            if (t.length > 0)
                root.duCache = JSON.parse(t);
        } catch (e) {}
    }

    function parseSensors(text: string): void {
        const temps = [], fans = [], curves = [];
        const fanNames = ["CPU", "GPU", "Mid"];
        for (const line of text.split("\n")) {
            const sp = line.indexOf(" ");
            const kind = line.slice(0, sp);
            const rest = line.slice(sp + 1);
            if (kind === "temp") {
                const f = rest.split("|");
                const chip = f[0], label = f[1].startsWith("temp") ? "" : f[1];
                if (chip === "coretemp" && label.startsWith("Core"))
                    continue;
                const names = { coretemp: "CPU package", k10temp: "CPU", zenpower: "CPU", acpitz: "Motherboard", iwlwifi_1: "Wi-Fi", nvme: "NVMe" };
                const pretty = chip === "coretemp" ? "CPU package" : label.length > 0 ? label : (names[chip] ?? chip);
                temps.push({ chip: names[chip] ?? chip, label: pretty, value: Number(f[2]) / 1000 });
            } else if (kind === "fan") {
                const f = rest.split("|");
                fans.push({ label: f[0] === "fan1_input" ? "ACPI" : f[0].replace(/_fan$|_input$/, "").toUpperCase(), rpm: Number(f[1]) });
            } else if (kind === "curve") {
                const f = rest.split(" ");
                curves.push({ name: fanNames[Number(f[0]) - 1] ?? f[0], enabled: f[1] === "1", points: (f[2] ?? "").split(",").filter(p => p.length > 0).map(p => ({ t: Number(p.split(":")[0]), pwm: Number(p.split(":")[1]) })) });
            }
        }
        const seen = {};
        root.temps = temps.filter(t => { const k = t.chip + "|" + t.label + "|" + t.value; if (seen[k]) return false; seen[k] = true; return true; });
        root.fans = fans;
        root.curves = curves;
    }

    function loadCurves(profile: string): void {
        root.curveProfile = profile;
        if (curvesProc.running) {
            root.curvePending = true;
            return;
        }
        curvesProc.command = [root.script, "curves", profile];
        curvesProc.running = true;
    }

    function parseCurves(text: string): void {
        const out = [];
        const re = /fan:\s*(\w+),\s*pwm:\s*\(([^)]*)\),\s*temp:\s*\(([^)]*)\),\s*enabled:\s*(\w+)/g;
        let m;
        while ((m = re.exec(text)) !== null) {
            const pwm = m[2].split(",").map(v => Number(v.trim())).filter(v => !isNaN(v));
            const temp = m[3].split(",").map(v => Number(v.trim())).filter(v => !isNaN(v));
            out.push({ name: m[1] === "MID" ? "Mid" : m[1], id: m[1].toLowerCase(), enabled: m[4] === "true", points: temp.map((t, i) => ({ t, pwm: pwm[i] ?? 0 })) });
        }
        root.profileCurves = out;
        root.curveBusy = false;
    }

    function applyCurve(profile: string, fan: string, points: var, enabled: bool): void {
        const data = points.map(p => `${Math.round(p.t)}c:${Math.round(p.pwm)}`).join(",");
        root.curveBusy = true;
        curveApplyProc.command = ["sh", "-c",
            `asusctl fan-curve --mod-profile "$1" --fan "$2" --data "$3" && asusctl fan-curve --mod-profile "$1" --fan "$2" --enable-fan-curve "$4"`,
            "sh", profile, fan, data, enabled ? "true" : "false"];
        curveApplyProc.running = true;
    }

    function setCurveEnabled(profile: string, fan: string, enabled: bool): void {
        root.curveBusy = true;
        curveApplyProc.command = ["asusctl", "fan-curve", "--mod-profile", profile, "--fan", fan, "--enable-fan-curve", enabled ? "true" : "false"];
        curveApplyProc.running = true;
    }

    function resetCurves(profile: string): void {
        root.curveBusy = true;
        curveApplyProc.command = ["asusctl", "fan-curve", "--mod-profile", profile, "--default"];
        curveApplyProc.running = true;
    }

    function refreshSensors(): void {
        if (!sensorsProc.running)
            sensorsProc.running = true;
    }

    function refreshProcs(): void {
        if (!procsProc.running)
            procsProc.running = true;
    }

    function refreshInfo(): void {
        if (!fetchProc.running)
            fetchProc.running = true;
    }

    function parseFetch(text: string): void {
        const info = {}, displays = [], gpus = [];
        for (const line of text.split("\n")) {
            const i = line.indexOf("|");
            if (i < 0)
                continue;
            const k = line.slice(0, i), v = line.slice(i + 1).trim();
            if (k === "display")
                displays.push(v);
            else if (k === "gpu")
                gpus.push(v);
            else
                info[k] = v;
        }
        root.info = info;
        root.displays = displays;
        root.gpus = gpus;
    }

    function copyText(text: string): void {
        Quickshell.execDetached(["sh", "-c", 'printf "%s" "$1" | wl-copy', "sh", text]);
    }

    function openMaintenance(): void {
        root.maintOpen = true;
        root.refreshFilesystems();
        gensProc.running = true;
    }

    function parseGens(text: string): void {
        const list = [];
        for (const line of text.split("\n")) {
            const f = line.trim().split(" ");
            if (f.length < 3)
                continue;
            list.push({ n: Number(f[0]), time: Number(f[1]) * 1000, current: f[2] === "1" });
        }
        root.generations = list.reverse();
    }

    function emptyTrash(): void {
        root.maintenanceBusy = true;
        maintProc.command = [root.script, "empty-trash"];
        maintProc.running = true;
    }

    function runInTerminal(cmd: string): void {
        Quickshell.execDetached(["wezterm", "-e", "sh", "-c", cmd + '; printf "\\nDone. Press enter to close."; read _']);
    }

    function setChargeLimit(limit: int): void {
        Quickshell.execDetached(["asusctl", "battery", "limit", String(Math.max(20, Math.min(100, limit)))]);
    }

    function chargeFullOnce(): void {
        Quickshell.execDetached(["asusctl", "battery", "oneshot"]);
    }

    function refreshFilesystems(): void {
        fsProc.running = true;
    }

    function parseFs(text: string): void {
        const list = [];
        for (const line of text.split("\n")) {
            const f = line.trim().split(/\s+/);
            if (f[0] === "trash") {
                root.trashSize = Number(f[1]) || 0;
                root.trashCount = Number(f[2]) || 0;
                continue;
            }
            if (f[0] === "nix") {
                root.nixGenerations = Number(f[1]) || 0;
                root.nixClosure = Number(f[2]) || 0;
                continue;
            }
            if (f.length < 5)
                continue;
            list.push({ mount: f[0], device: f[1], type: f[2], used: Number(f[3]), size: Number(f[4]) });
        }
        root.filesystems = list;
    }


    function notify(id: string, title: string, body: string, urgency: string): void {
        Quickshell.execDetached(["notify-send", "-a", "System Monitor", "-i", "utilities-system-monitor", "-u", urgency, "-h", "string:x-canonical-private-synchronous:sysmon-" + id, title, body]);
    }

    property bool alertsPrimed: false

    function alert(id: string, active: bool, title: string, body: string, urgency: string): void {
        const was = root.alertState[id] ?? false;
        if (active === was)
            return;
        const st = Object.assign({}, root.alertState);
        st[id] = active;
        root.alertState = st;
        if (active && root.alertsPrimed)
            root.notify(id, title, body, urgency);
    }

    function sustained(hist: var, n: int, limit: real): bool {
        if (hist.length < n)
            return false;
        return hist.slice(-n).every(v => v >= limit);
    }

    function checkAlerts(): void {
        const n = root.open ? 30 : 3;
        root.alert("cpu", root.sustained(root.cpuHist, n, 90), "High CPU usage", `CPU has been above 90% for 30 seconds (${root.cpu.toFixed(0)}%)`, "normal");
        root.alert("mem", root.memPct >= 90, "Memory almost full", `${root.fmtBytes(root.memAvail, 1)} available of ${root.fmtBytes(root.memTotal, 1)}`, "critical");
        root.alert("gpu", root.gpuState === "active" && root.sustained(root.gpuHist, n, 95), "High GPU usage", `${root.gpuName} at ${root.gpuUtil.toFixed(0)}%`, "normal");
        root.alert("vram", root.gpuState === "active" && root.gpuMemTotal > 0 && root.gpuMemUsed / root.gpuMemTotal >= 0.9, "VRAM almost full", `${root.fmtBytes(root.gpuMemUsed, 1)} of ${root.fmtBytes(root.gpuMemTotal, 1)} used`, "normal");
        root.alert("temp", root.cpuTemp >= 95, "CPU running hot", `Package temperature ${root.cpuTemp.toFixed(0)}°C`, "critical");
        if (root.batPresent) {
            const dis = root.batStatus === "Discharging";
            root.alert("unplugged", !root.acOnline, "On battery power", `${root.batCapacity}% · about ${root.fmtHours(root.batTimeLeft)} remaining`, "low");
            root.alert("plugged", root.acOnline, "Charger connected", root.batStatus === "Charging" ? `Charging from ${root.batCapacity}%` : `${root.batCapacity}% · ${root.batStatus.toLowerCase()}`, "low");
            root.alert("low", dis && root.batCapacity <= 25 && root.batCapacity > 10, "Battery low", `${root.batCapacity}% remaining · about ${root.fmtHours(root.batTimeLeft)}`, "normal");
            root.alert("critical", dis && root.batCapacity <= 10, "Battery critically low", `${root.batCapacity}% remaining — plug in now`, "critical");
            root.alert("full", root.batStatus === "Full" || (root.acOnline && root.batThreshold < 100 && root.batCapacity >= root.batThreshold), "Battery charged", `${root.batCapacity}%`, "low");
        }
        root.alertsPrimed = true;
    }

    function endProcess(pid: int, force: bool): void {
        root.endProcesses([pid], force);
    }

    function endProcesses(pids: var, force: bool): void {
        const list = pids.filter(p => p > 0).map(String);
        if (list.length > 0)
            Quickshell.execDetached(["kill", force ? "-KILL" : "-TERM"].concat(list));
    }

    function parseSample(text: string): void {
        const now = Date.now();
        const dt = root.lastSample > 0 ? (now - root.lastSample) / 1000 : 0;
        root.lastSample = now;
        const cores = [];
        let bat = {};
        for (const line of text.split("\n")) {
            const f = line.trim().split(/\s+/);
            const k = f[0];
            if (k === "cpu") {
                root.parseCpu(f.slice(1).map(Number));
            } else if (/^cpu\d+$/.test(k)) {
                cores.push(f.slice(1).map(Number));
            } else if (k === "load") {
                root.load = f.slice(1).join(" ");
            } else if (k === "uptime") {
                root.uptime = Number(f[1]);
            } else if (k === "mem_memtotal") {
                root.memTotal = Number(f[1]) * 1024;
            } else if (k === "mem_memavailable") {
                root.memAvail = Number(f[1]) * 1024;
            } else if (k === "mem_cached") {
                root.memCached = Number(f[1]) * 1024;
            } else if (k === "mem_swaptotal") {
                root.swapTotal = Number(f[1]) * 1024;
            } else if (k === "mem_swapfree") {
                root.swapFree = Number(f[1]) * 1024;
            } else if (k === "net") {
                root.parseRate(f, dt, "net");
                root.netIface = f[3] ?? "";
            } else if (k === "disk") {
                root.parseRate(f, dt, "disk");
            } else if (k === "freq") {
                root.freq = Number(f[1]);
            } else if (k === "model") {
                root.cpuModel = f.slice(1).join(" ").replace(/\(R\)|\(TM\)|CPU|Processor/g, "").replace(/\s+/g, " ").trim();
            } else if (k === "cputemp") {
                root.cpuTemp = Number(f[1]) / 1000;
            } else if (k === "ac") {
                root.acOnline = f[1] === "1";
            } else if (k === "nvidia") {
                root.gpuState = f[1] ?? "unknown";
            } else if (k === "igpu") {
                root.igpuFreq = Number(f[1]);
                root.igpuMax = Number(f[2]);
            } else if (k === "rootfs") {
                root.rootUsed = Number(f[1]);
                root.rootSize = Number(f[2]);
            } else if (k.startsWith("bat_")) {
                bat[k.slice(4)] = f.slice(1).join(" ");
            }
        }
        root.parseCores(cores);
        root.parseBattery(bat);
        root.memHist = root.push(root.memHist, root.memPct);
        root.cpuTempHist = root.push(root.cpuTempHist, root.cpuTemp);
        if (root.gpuState === "active")
            gpuProc.running = true;
        else if (root.gpuState !== "none") {
            root.gpuHist = root.push(root.gpuHist, 0);
            root.gpuTempHist = root.push(root.gpuTempHist, 0);
            root.gpuProcs = [];
            root.alert("dgpu", false, "", "", "low");
        }
        root.checkAlerts();
    }

    function cpuBusy(cur: var, prev: var): real {
        const total = (a) => a.slice(0, 8).reduce((x, y) => x + y, 0);
        const idle = (a) => a[3] + a[4];
        const dt = total(cur) - total(prev);
        return dt > 0 ? Math.max(0, Math.min(100, (1 - (idle(cur) - idle(prev)) / dt) * 100)) : 0;
    }

    function parseCpu(cur: var): void {
        if (root.prevCpu) {
            root.cpu = root.cpuBusy(cur, root.prevCpu);
            root.cpuHist = root.push(root.cpuHist, root.cpu);
        }
        root.prevCpu = cur;
    }

    function parseCores(cores: var): void {
        root.cpuCount = Math.max(1, cores.length);
        if (root.prevCores && root.prevCores.length === cores.length)
            root.cores = cores.map((c, i) => root.cpuBusy(c, root.prevCores[i]));
        root.prevCores = cores;
    }

    function parseRate(f: var, dt: real, kind: string): void {
        const a = Number(f[1]);
        const b = Number(f[2]);
        const prev = kind === "net" ? root.prevNet : root.prevDisk;
        const ok = prev && dt > 0;
        const ra = ok ? Math.max(0, (a - prev[0]) / dt) : 0;
        const rb = ok ? Math.max(0, (b - prev[1]) / dt) : 0;
        if (kind === "net") {
            root.prevNet = [a, b];
            root.netRx = ra;
            root.netTx = rb;
            root.netRxTotal = a;
            root.netTxTotal = b;
            if (ok) {
                root.netRxHist = root.push(root.netRxHist, ra);
                root.netTxHist = root.push(root.netTxHist, rb);
            }
        } else {
            root.prevDisk = [a, b];
            root.diskRead = ra;
            root.diskWrite = rb;
            root.diskReadTotal = a;
            root.diskWriteTotal = b;
            if (ok) {
                root.diskReadHist = root.push(root.diskReadHist, ra);
                root.diskWriteHist = root.push(root.diskWriteHist, rb);
            }
        }
    }

    function parseBattery(b: var): void {
        root.batPresent = "status" in b;
        if (!root.batPresent)
            return;
        const v = Number(b.voltage_now ?? 0) / 1e6;
        const toWh = (uAh) => Number(uAh) / 1e6 * v;
        root.batStatus = b.status;
        root.batCapacity = Number(b.capacity ?? 0);
        root.batVoltage = v;
        if ("energy_now" in b) {
            root.batEnergyNow = Number(b.energy_now) / 1e6;
            root.batEnergyFull = Number(b.energy_full ?? 0) / 1e6;
            root.batEnergyDesign = Number(b.energy_full_design ?? 0) / 1e6;
        } else {
            root.batEnergyNow = toWh(b.charge_now ?? 0);
            root.batEnergyFull = toWh(b.charge_full ?? 0);
            root.batEnergyDesign = toWh(b.charge_full_design ?? 0);
        }
        if ("power_now" in b)
            root.batPower = Math.abs(Number(b.power_now)) / 1e6;
        else
            root.batPower = Math.abs(Number(b.current_now ?? 0)) / 1e6 * v;
        root.batCycles = Number(b.cycle_count ?? 0);
        root.batThreshold = Number(b.charge_control_end_threshold ?? 100);
        root.batModel = b.model_name ?? "";
        root.batVendor = (b.manufacturer ?? "").trim();
        root.batTech = b.technology ?? "";
        root.batPowerHist = root.push(root.batPowerHist, root.batPower);
        root.batCapHist = root.push(root.batCapHist, root.batCapacity);
        root.trackSession();
    }

    property bool batOnBattery: false

    function trackSession(): void {
        const now = Date.now();
        const onBat = !root.acOnline;
        if (root.batSince === 0 || onBat !== root.batOnBattery) {
            root.batOnBattery = onBat;
            root.batSince = now;
            root.batApps = {};
            root.batSession = [];
            root.saveBattery();
        }
        const last = root.batSession[root.batSession.length - 1];
        if (!last || now - last[0] >= 60000)
            root.batSession = root.batSession.concat([[now, root.batCapacity, Math.round(root.batPower * 100) / 100]]);
    }

    function attribute(deltas: var, busy: real): void {
        const now = Date.now();
        const dt = root.lastProcTime > 0 ? (now - root.lastProcTime) / 1000 : 0;
        root.lastProcTime = now;
        if (dt <= 0 || dt > 120 || busy <= 0)
            return;
        const wh = root.batStatus === "Discharging" ? root.batPower * dt / 3600 : 0;
        const apps = Object.assign({}, root.batApps);
        for (const name in deltas) {
            const e = apps[name] ? Object.assign({}, apps[name]) : { wh: 0, cpu: 0 };
            e.wh += wh * deltas[name] / busy;
            e.cpu += deltas[name] / 100;
            apps[name] = e;
        }
        root.batApps = apps;
        if (now - root.lastBatSave > 60000)
            root.saveBattery();
    }

    function saveBattery(): void {
        root.lastBatSave = Date.now();
        Quickshell.execDetached(["mkdir", "-p", root.cacheDir]);
        batteryFile.setText(JSON.stringify({ since: root.batSince, onBattery: root.batOnBattery, apps: root.batApps, session: root.batSession }));
    }

    function loadBattery(): void {
        try {
            const d = JSON.parse(batteryFile.text());
            if (d.since > 0 && Object.keys(root.batApps).length === 0) {
                root.batSince = d.since;
                root.batOnBattery = d.onBattery ?? true;
                root.batApps = d.apps ?? {};
                root.batSession = d.session ?? [];
            }
        } catch (e) {}
    }

    function parseGpu(text: string): void {
        const parts = text.split("\n--\n");
        const f = (parts[0] ?? "").trim().split(",").map(s => s.trim());
        const procs = [];
        for (const line of (parts[1] ?? "").split("\n")) {
            const g = line.split(",").map(s => s.trim());
            if (g.length >= 3 && g[0].length > 0)
                procs.push({ pid: Number(g[0]), name: g[1].slice(g[1].lastIndexOf("/") + 1), mem: Number(g[2]) * 1048576 });
        }
        root.gpuProcs = procs;
        if (f.length < 6)
            return;
        root.gpuName = f[0].replace(/^NVIDIA /, "");
        root.gpuUtil = Number(f[1]) || 0;
        root.gpuTemp = Number(f[2]) || 0;
        root.gpuMemUsed = (Number(f[3]) || 0) * 1048576;
        root.gpuMemTotal = (Number(f[4]) || 0) * 1048576;
        root.gpuPower = Number(f[5]) || 0;
        root.gpuHist = root.push(root.gpuHist, root.gpuUtil);
        root.gpuTempHist = root.push(root.gpuTempHist, root.gpuTemp);
        root.alert("dgpu", true, "Discrete GPU woke up", procs.length > 0 ? `Used by ${procs.map(p => p.name).join(", ")}` : "No process is listed yet — check the GPU tab", "low");
    }

    function parseProcs(text: string): void {
        const lines = text.split("\n");
        let total = 0;
        const next = {};
        const list = [];
        const deltas = {};
        let busy = 0;
        let threads = 0;
        for (const line of lines) {
            if (line.startsWith("total ")) {
                total = Number(line.slice(6));
                continue;
            }
            const m = line.match(/^(\d+) (\S+) (\d+) (\d+) (\d+) (\S) (.*)$/);
            if (!m)
                continue;
            const pid = Number(m[1]);
            const ticks = Number(m[3]);
            next[pid] = ticks;
            const dTotal = total - root.prevProcTotal;
            const prev = root.prevProcs[pid];
            const delta = prev !== undefined ? Math.max(0, ticks - prev) : 0;
            const cpu = prev !== undefined && dTotal > 0 ? delta / dTotal * 100 : 0;
            threads += Number(m[4]);
            const name = m[7];
            if (delta > 0) {
                const key = Number(m[5]) === 0 ? "Kernel" : name;
                deltas[key] = (deltas[key] ?? 0) + delta;
                busy += delta;
            }
            list.push({ pid, user: m[2], cpu, threads: Number(m[4]), mem: Number(m[5]) * 4096, pstate: m[6], name });
        }
        if (root.prevProcTotal > 0)
            root.attribute(deltas, busy);
        root.prevProcs = next;
        root.prevProcTotal = total;
        root.procCount = list.length;
        root.threadCount = threads;
        root.procs = list;
        if (root.selectedPid > 0 && !(root.selectedPid in next))
            root.selectedPid = 0;
    }

    onOpenChanged: {
        if (open) {
            root.lastSample = 0;
            root.prevCpu = null;
            root.prevCores = null;
            root.prevNet = null;
            root.prevDisk = null;
            root.prevProcs = {};
            root.cpuHist = [];
            root.memHist = [];
            root.gpuHist = [];
            root.netRxHist = [];
            root.netTxHist = [];
            root.diskReadHist = [];
            root.diskWriteHist = [];
            root.batPowerHist = [];
            root.batCapHist = [];
            root.cpuTempHist = [];
            root.gpuTempHist = [];
            sampleProc.running = true;
            procsProc.running = true;
        }
    }

    FileView {
        id: hostFile
        path: "/etc/hostname"
    }

    Process {
        id: sampleProc
        command: [root.script, "sample"]
        stdout: StdioCollector {
            onStreamFinished: root.parseSample(text)
        }
    }

    Process {
        id: gpuProc
        command: [root.script, "gpu"]
        stdout: StdioCollector {
            onStreamFinished: root.parseGpu(text)
        }
    }

    Process {
        id: curvesProc
        stdout: StdioCollector {
            onStreamFinished: root.parseCurves(text)
        }
        onExited: {
            if (root.curvePending) {
                root.curvePending = false;
                curvesProc.command = [root.script, "curves", root.curveProfile];
                curvesProc.running = true;
            }
        }
    }

    Process {
        id: curveApplyProc
        onExited: {
            root.loadCurves(root.curveProfile);
            root.refreshSensors();
        }
    }

    Process {
        id: sensorsProc
        command: [root.script, "sensors"]
        stdout: StdioCollector {
            onStreamFinished: root.parseSensors(text)
        }
    }

    Process {
        id: filesProc
        stdout: StdioCollector {
            onStreamFinished: root.parseFiles(text)
        }
    }

    Process {
        id: rmProc
        onExited: {
            const cache = Object.assign({}, root.duCache);
            for (const k in cache)
                if (k === root.duPath || k.startsWith(root.duPath + "/"))
                    delete cache[k];
            root.duCache = cache;
            if (root.duFiles)
                root.showFiles(root.duPath);
            else
                root.scanDir(root.duPath);
        }
    }

    Process {
        id: fetchProc
        command: [root.script, "fetch"]
        stdout: StdioCollector {
            onStreamFinished: root.parseFetch(text)
        }
    }

    Process {
        id: gensProc
        command: [root.script, "gens"]
        stdout: StdioCollector {
            onStreamFinished: root.parseGens(text)
        }
    }

    Process {
        id: maintProc
        onExited: {
            root.maintenanceBusy = false;
            root.refreshFilesystems();
        }
    }

    Process {
        id: fsProc
        command: [root.script, "fs"]
        stdout: StdioCollector {
            onStreamFinished: root.parseFs(text)
        }
    }

    FileView {
        id: batteryFile
        path: root.cacheDir + "/battery.json"
        printErrors: false
        onLoaded: root.loadBattery()
    }

    FileView {
        id: duCacheFile
        path: root.cacheDir + "/du.json"
        printErrors: false
        onLoaded: root.loadDuCache()
    }

    Process {
        id: duProc
        stdout: StdioCollector {
            onStreamFinished: root.parseDu(text)
        }
        onExited: {
            if (root.pendingScan.length > 0) {
                const next = root.pendingScan;
                root.pendingScan = "";
                root.scanRoot = next;
                duProc.command = [root.script, "du", next];
                duProc.running = true;
            }
        }
    }

    Process {
        id: procsProc
        command: [root.script, "procs"]
        stdout: StdioCollector {
            onStreamFinished: root.parseProcs(text)
        }
    }

    Timer {
        interval: 1000
        running: root.open
        repeat: true
        onTriggered: if (!sampleProc.running) sampleProc.running = true
    }

    Timer {
        interval: 2000
        running: root.open
        repeat: true
        onTriggered: if (!procsProc.running) procsProc.running = true
    }

    Timer {
        interval: 10000
        running: !root.open
        repeat: true
        triggeredOnStart: true
        onTriggered: {
            if (!sampleProc.running)
                sampleProc.running = true;
            if (!procsProc.running)
                procsProc.running = true;
        }
    }

    Timer {
        interval: 2000
        running: root.open && root.tab === "sensors"
        repeat: true
        triggeredOnStart: true
        onTriggered: root.refreshSensors()
    }

    Timer {
        interval: 15000
        running: root.open && root.tab === "storage"
        repeat: true
        triggeredOnStart: true
        onTriggered: root.refreshFilesystems()
    }
}
