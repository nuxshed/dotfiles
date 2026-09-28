import QtQuick
import Quickshell
import Quickshell.Hyprland
import "../../../config"
import "../../../services"
import ".."

Provider {
    id: root

    name: "commands"
    label: "Commands"
    prefix: ">"
    icon: "chevron_right"
    keywords: ["commands", "actions"]
    weight: 0.95

    readonly property var commands: [
        { id: "lock", title: "Lock screen", icon: "lock", keywords: ["lock", "session"], act: () => Lock.lock() },
        { id: "suspend", title: "Suspend", icon: "brightness_3", keywords: ["sleep", "suspend"], act: () => Quickshell.execDetached(["systemctl", "suspend"]) },
        { id: "reboot", title: "Reboot", icon: "replay", keywords: ["restart", "reboot"], act: () => Quickshell.execDetached(["systemctl", "reboot"]) },
        { id: "poweroff", title: "Power off", icon: "power_settings_new", keywords: ["shutdown", "poweroff", "halt"], act: () => Quickshell.execDetached(["systemctl", "poweroff"]) },
        { id: "logout", title: "Log out", icon: "exit_to_app", keywords: ["exit", "logout", "quit"], act: () => Hyprland.dispatch("hl.dsp.exit()") },
        { id: "reload", title: "Reload shell", icon: "refresh", keywords: ["reload", "restart", "quickshell"], act: () => Quickshell.reload(true) },
        { id: "shot-region", title: "Screenshot region", icon: "crop_free", keywords: ["screenshot", "capture", "region"], act: () => Capture.region("copy") },
        { id: "shot-full", title: "Screenshot screen", icon: "desktop_windows", keywords: ["screenshot", "fullscreen"], act: () => Capture.fullscreen() },
        { id: "shot-annotate", title: "Screenshot & annotate", icon: "edit", keywords: ["annotate", "draw", "screenshot"], act: () => Capture.region("annotate") },
        { id: "pin", title: "Pin region", icon: "picture_in_picture_alt", keywords: ["pin", "region"], act: () => Capture.region("pin") },
        { id: "colour", title: "Pick colour", icon: "colorize", keywords: ["colour", "color", "picker", "eyedropper"], act: () => Capture.pickColor() },
        { id: "record", title: "Record screen", icon: "videocam", keywords: ["record", "screen", "video"], act: () => Recorder.kind === "screen" ? Recorder.stop() : Recorder.startScreen("") },
        { id: "booth", title: "Photo Booth", icon: "photo_camera", keywords: ["photo", "booth", "camera", "webcam", "selfie"], act: () => Booth.show() },
        { id: "record-voice", title: "Record voice", icon: "mic", keywords: ["record", "voice", "audio"], act: () => Recorder.kind === "voice" ? Recorder.stop() : Recorder.startVoice() },
        { id: "clear-notifs", title: "Clear notifications", icon: "notifications_off", keywords: ["clear", "notifications", "dismiss"], act: () => Notifications.clear() },
        { id: "reindex", title: "Rebuild file index", icon: "find_in_page", keywords: ["index", "reindex", "files"], act: () => Quickshell.execDetached(["systemctl", "--user", "start", "spotlight-index.service"]) }
    ]

    function itemFor(c, score) {
        return {
            key: "cmd:" + c.id,
            kind: "commands",
            section: "Commands",
            icon: c.icon,
            title: c.title,
            subtitle: "Command",
            score: score,
            activate: c.act
        }
    }

    function itemForId(id) {
        const c = root.commands.find(c => c.id === id)
        return c ? root.itemFor(c, 0.5) : null
    }

    function search(text) {
        const out = []

        for (const c of root.commands) {
            const s = Fuzzy.best(text, [c.title].concat(c.keywords))
            if (s >= 0)
                out.push(root.itemFor(c, s))
        }

        root.results = out
    }
}
