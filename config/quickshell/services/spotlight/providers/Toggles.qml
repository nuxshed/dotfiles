import QtQuick
import Quickshell
import "../../../config"
import "../../../services"
import ".."

Provider {
    id: root

    name: "toggles"
    label: "System"
    prefix: "sys"
    icon: "tune"
    keywords: ["settings", "system", "toggles", "quick settings"]
    weight: 1.05
    cap: 4

    function toggleItem(t, want, score) {
        const on = t.on()
        const turnOn = want === null ? !on : want
        return {
            key: "sys:" + t.id,
            kind: "toggles",
            section: "System",
            icon: t.icon,
            title: t.label ? t.label(turnOn) : (turnOn ? "Turn on " : "Turn off ") + t.title,
            subtitle: t.detail ? t.detail() : t.title,
            badge: t.label ? (on ? "Muted" : "Live") : on ? "On" : "Off",
            dot: on && !t.label ? Colors.green : "",
            score: score,
            pinned: want !== null,
            activate: () => t.set(turnOn)
        }
    }

    readonly property var toggles: [
        {
            id: "wifi", title: "Wi-Fi", icon: "wifi", keywords: ["wifi", "wi-fi", "wlan", "wireless", "internet"],
            on: () => Network.wifiEnabled, detail: () => Network.ssid ? "Connected to " + Network.ssid : "Not connected",
            set: v => Network.enableWifi(v)
        },
        {
            id: "bluetooth", title: "Bluetooth", icon: "bluetooth", keywords: ["bluetooth", "bt"],
            on: () => Bluetooth.enabled, detail: () => Bluetooth.connectedDevices.length ? Bluetooth.connectedDevices.map(d => d.name).join(", ") : "No devices connected",
            set: v => { if (v !== Bluetooth.enabled) Bluetooth.toggleEnabled() }
        },
        {
            id: "vpn", title: "VPN", icon: "vpn_key", keywords: ["vpn", Vpn.name],
            on: () => Vpn.active, detail: () => Vpn.name + (Vpn.error ? " · " + Vpn.error : ""),
            set: v => {
                if (v === Vpn.active)
                    return
                Prompt.ask({ title: v ? "Start VPN" : "Stop VPN", subtitle: Vpn.name, placeholder: "sudo password", action: v ? "Start" : "Stop", onSubmit: p => Vpn.toggle(p) })
            }
        },
        {
            id: "dnd", title: "Do Not Disturb", icon: "notifications_off", keywords: ["dnd", "do not disturb", "focus", "silence notifications"],
            on: () => Notifications.dnd, detail: () => "Only critical notifications pop up",
            set: v => Notifications.dnd = v
        },
        {
            id: "mute", title: "Sound", icon: "volume_off", label: v => v ? "Mute sound" : "Unmute sound", keywords: ["mute", "sound", "speaker", "audio"],
            on: () => Audio.sink?.audio?.muted ?? false, detail: () => Audio.sink ? Audio.name(Audio.sink) : "No output",
            set: v => { if (Audio.sink && v !== Audio.sink.audio.muted) Audio.toggleMute(Audio.sink) }
        },
        {
            id: "mic", title: "Microphone", icon: "mic_off", label: v => v ? "Mute microphone" : "Unmute microphone", keywords: ["mic", "microphone", "mute mic"],
            on: () => Audio.source?.audio?.muted ?? false, detail: () => Audio.source ? Audio.name(Audio.source) : "No input",
            set: v => { if (Audio.source && v !== Audio.source.audio.muted) Audio.toggleMute(Audio.source) }
        }
    ]

    readonly property var profiles: [
        { name: "Quiet", icon: "brightness_3", keywords: ["quiet", "silent", "battery saver", "eco", "power saver"] },
        { name: "Balanced", icon: "tune", keywords: ["balanced", "normal"] },
        { name: "Performance", icon: "flash_on", keywords: ["performance", "turbo", "gaming", "fast"] }
    ]

    function level(label, icon, value, apply, section) {
        const v = Math.max(0, Math.min(100, value))
        return {
            key: "sys:" + label.toLowerCase(),
            kind: "toggles",
            section: "System",
            icon: icon,
            title: "Set " + label.toLowerCase() + " to " + v + "%",
            subtitle: section,
            score: 1.55,
            pinned: true,
            activate: () => apply(v / 100)
        }
    }

    function search(text, scoped) {
        const q = text.trim().toLowerCase()
        const out = []
        if (!q && !scoped) {
            root.results = []
            return
        }

        const vol = q.match(/^(?:vol(?:ume)?|sound)\s+(\d{1,3})%?$/)
        if (vol && Audio.sink)
            out.push(root.level("Volume", "volume_up", parseInt(vol[1]), v => Audio.setVolume(Audio.sink, v), Audio.name(Audio.sink) + " · now " + Math.round((Audio.sink.audio?.volume ?? 0) * 100) + "%"))
        const bri = q.match(/^(?:bri(?:ght(?:ness)?)?|screen)\s+(\d{1,3})%?$/)
        if (bri)
            out.push(root.level("Brightness", "brightness_6", parseInt(bri[1]), v => Backlight.set(v), "now " + Math.round(Backlight.value * 100) + "%"))

        const out2 = q.match(/^(?:output|speaker|audio|sound)\s+(?:to\s+)?(.+)$/)
        for (const s of Audio.sinks) {
            const name = Audio.name(s)
            const sc = out2 ? Fuzzy.score(out2[1], name) : (scoped || q.length >= 3 ? Fuzzy.best(q, ["output " + name, "audio output"]) : -1)
            if (sc < (out2 ? 0 : 0.6))
                continue
            const current = s === Audio.sink
            out.push({
                key: "sys:sink:" + name,
                kind: "toggles",
                section: "Audio output",
                icon: Audio.deviceIcon(s),
                title: current ? name : "Switch output to " + name,
                subtitle: Audio.detail(s),
                badge: current ? "Current" : "",
                dot: current ? Colors.green : "",
                score: out2 ? 1.2 + sc * 0.1 : sc,
                activate: current ? null : () => Audio.setDefault(s)
            })
        }

        const m = q.match(/^(?:turn\s+)?(.+?)\s+(on|off|enable|disable|toggle)$/) ?? q.match(/^(?:turn|switch)\s+(on|off)\s+(.+)$/)
        let subject = q
        let want = null
        if (m) {
            const verbFirst = m[1] === "on" || m[1] === "off"
            subject = verbFirst ? m[2] : m[1]
            const verb = verbFirst ? m[1] : m[2]
            want = verb === "on" || verb === "enable" ? true : verb === "off" || verb === "disable" ? false : null
        }

        for (const t of root.toggles) {
            if (t.id === "vpn" && !Vpn.initialized)
                continue
            const s = subject ? Fuzzy.best(subject, [t.title].concat(t.keywords)) : 0.5
            if (s < (scoped || m ? 0.5 : 0.8))
                continue
            const w = m && want !== null && t.label && !/mute/.test(subject) ? !want : want
            out.push(root.toggleItem(t, m ? w : null, (m ? 1.5 : s)))
        }

        if (Asusctl.isAvailable) {
            for (const p of root.profiles) {
                const s = q ? Fuzzy.best(q.replace(/\s*(mode|profile)$/, ""), [p.name].concat(p.keywords).concat(["power profile", "power mode"])) : 0.4
                if (s < (scoped ? 0.4 : 0.8))
                    continue
                const current = Asusctl.activeProfile === p.name
                out.push({
                    key: "sys:profile:" + p.name,
                    kind: "toggles",
                    section: "Power profile",
                    icon: p.icon,
                    title: current ? p.name + " mode" : "Switch to " + p.name.toLowerCase() + " mode",
                    subtitle: "Power profile",
                    badge: current ? "Current" : "",
                    dot: current ? Colors.green : "",
                    score: s,
                    activate: current ? null : () => Asusctl.setProfile(p.name)
                })
            }
        }

        root.results = out
    }
}
