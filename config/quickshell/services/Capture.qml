pragma Singleton
pragma ComponentBehavior: Bound

import QtQuick
import Quickshell
import Quickshell.Io
import Quickshell.Hyprland

Singleton {
    id: root

    readonly property string home: Quickshell.env("HOME")
    readonly property string tmpDir: "/tmp/qs-capture"
    readonly property string shotDir: home + "/Pictures/Screenshots"

    property string freezePath: ""
    property string mode: ""
    property var targetScreen: null
    property bool busy: false
    property bool shielding: false
    property var pendingCapture: null
    property string lastColour: ""
    property bool scrolling: false
    property rect scrollRect
    property int scrollFrames: 0

    readonly property string uploadEndpoint: "https://uguu.se/upload"

    signal regionReady(var screen, string path)
    signal annotateReady(string path)
    signal pinReady(string path)

    function focusedScreen() {
        const name = Hyprland.focusedMonitor?.name ?? ""
        for (const s of Quickshell.screens)
            if (s.name === name)
                return s
        return Quickshell.screens[0] ?? null
    }

    function stamp() {
        const d = new Date()
        const p = n => String(n).padStart(2, "0")
        return `${d.getFullYear()}${p(d.getMonth() + 1)}${p(d.getDate())}-${p(d.getHours())}${p(d.getMinutes())}${p(d.getSeconds())}`
    }

    function notify(title, body, image) {
        const args = ["notify-send", "-a", "Shell", "-i", "camera-photo"]
        if (image)
            args.push("-h", "string:image-path:file://" + image)
        args.push(title, body ?? "")
        Quickshell.execDetached(args)
    }

    function sh(script) {
        return ["bash", "-c", script]
    }

    function shield(capture) {
        pendingCapture = capture
        shielding = true
        settleTimer.stop()
        shieldTimeout.restart()
    }

    function shieldEntered() {
        if (shielding && pendingCapture)
            settleTimer.restart()
    }

    function fireCapture() {
        shieldTimeout.stop()
        settleTimer.stop()
        const capture = pendingCapture
        pendingCapture = null
        if (capture)
            capture()
    }

    function region(actionMode) {
        if (busy)
            return
        const screen = focusedScreen()
        if (!screen)
            return
        busy = true
        mode = actionMode
        targetScreen = screen
        freezePath = `${tmpDir}/freeze-${Date.now()}.png`
        shield(() => freezeProc.exec(sh(`mkdir -p '${tmpDir}' && grim -l 0 -o '${screen.name}' '${freezePath}'`)))
    }

    function fullscreen() {
        const screen = focusedScreen()
        if (!screen)
            return
        const out = `${shotDir}/screenshot_${stamp()}.png`
        actionProc.pending = "copy"
        actionProc.payload = out
        shield(() => actionProc.exec(sh(`mkdir -p '${shotDir}' && grim -o '${screen.name}' '${out}' && wl-copy -t image/png < '${out}'`)))
    }

    function cancel() {
        busy = false
        shielding = false
        pendingCapture = null
        cleanup(freezePath)
        freezePath = ""
    }

    function cleanup(path) {
        if (path)
            Quickshell.execDetached(sh(`rm -f '${path}'`))
    }

    function crop(x, y, w, h) {
        busy = false
        if (w < 2 || h < 2) {
            cancel()
            return
        }

        const src = freezePath
        const filter = `crop=${w}:${h}:${x}:${y}`
        const base = `ffmpeg -y -loglevel error -i '${src}' -vf ${filter} -frames:v 1`

        if (mode === "copy") {
            const out = `${shotDir}/screenshot_${stamp()}.png`
            actionProc.pending = "copy"
            actionProc.payload = out
            actionProc.exec(sh(`mkdir -p '${shotDir}' && ${base} '${out}' && wl-copy -t image/png < '${out}'; rm -f '${src}'`))
        } else if (mode === "annotate" || mode === "pin") {
            const out = `${tmpDir}/${mode}-${Date.now()}.png`
            actionProc.pending = mode
            actionProc.payload = out
            actionProc.exec(sh(`${base} '${out}'; rm -f '${src}'`))
        } else if (mode === "ocr") {
            const out = `${tmpDir}/ocr-${Date.now()}.png`
            actionProc.pending = "ocr"
            actionProc.payload = ""
            actionProc.exec(sh(`set -o pipefail; ${base} '${out}' && tesseract '${out}' stdout 2>/dev/null | wl-copy; rc=$?; rm -f '${src}' '${out}'; exit $rc`))
        } else if (mode === "upload") {
            const out = `${tmpDir}/upload-${Date.now()}.png`
            actionProc.pending = "upload"
            actionProc.payload = ""
            actionProc.exec(sh(`set -o pipefail; ${base} '${out}' && url=$(curl -sf --max-time 30 -F 'files[]=@${out}' '${uploadEndpoint}' | jq -r '.files[0].url') && [ -n "$url" ] && [ "$url" != null ] && printf '%s' "$url" | wl-copy && printf '%s' "$url"; rc=$?; rm -f '${src}' '${out}'; exit $rc`))
        } else {
            cleanup(src)
        }
        freezePath = ""
    }

    function scroll(rect) {
        busy = false
        cleanup(freezePath)
        freezePath = ""
        scrollRect = rect
        scrollFrames = 0
        scrolling = true
        const out = `${shotDir}/screenshot_${stamp()}.png`
        scrollProc.exec(["bash", "-c", `mkdir -p '${shotDir}' && exec "$HOME/.bin/qs-scrollshot" "$1" "$2"`, "qs-scrollshot",
            `${Math.round(rect.x)},${Math.round(rect.y)} ${Math.round(rect.width)}x${Math.round(rect.height)}`, out])
    }

    function finishScroll(save) {
        scrollProc.write(save ? "done\n" : "cancel\n")
    }

    function takeColour(output) {
        const match = (output ?? "").match(/#?[0-9a-fA-F]{6}\b/)
        if (!match)
            return
        const hex = match[0].startsWith("#") ? match[0] : "#" + match[0]
        if (hex === lastColour)
            return
        lastColour = hex
        Quickshell.execDetached(sh(`printf '%s' '${hex}' | wl-copy`))
        notify("Colour picked", hex)
    }

    function pickColor() {
        lastColour = ""

        colorProc.exec(["hyprpicker", "-b", "-f", "hex"])
    }

    function openDir(path) {
        Quickshell.execDetached(sh(`mkdir -p '${path}' && xdg-open '${path}'`))
    }

    Timer {
        id: settleTimer
        interval: 50
        onTriggered: root.fireCapture()
    }

    Timer {
        id: shieldTimeout
        interval: 300
        onTriggered: root.fireCapture()
    }

    Process {
        id: freezeProc
        onExited: code => {
            root.shielding = false
            if (code === 0)
                root.regionReady(root.targetScreen, root.freezePath)
            else
                root.cancel()
        }
    }

    Process {
        id: actionProc

        property string pending: ""
        property string payload: ""

        stdout: StdioCollector {
            onStreamFinished: {
                const url = text.trim()
                if (actionProc.pending === "upload" && url)
                    root.notify("Link copied", url)
            }
        }

        onExited: code => {
            root.shielding = false
            if (code !== 0) {
                if (pending === "upload")
                    root.notify("Upload failed", "Could not reach the upload host")
                else if (pending === "ocr")
                    root.notify("No text found", "Nothing was recognised in that region")
                else
                    root.notify("Capture failed", pending)
                return
            }
            if (pending === "copy")
                root.notify("Screenshot saved", payload.replace(root.home, "~"), payload)
            else if (pending === "annotate")
                root.annotateReady(payload)
            else if (pending === "pin")
                root.pinReady(payload)
            else if (pending === "ocr")
                root.notify("Text copied", "Recognised text is on the clipboard")

        }
    }

    Process {
        id: scrollProc

        property string saved: ""

        stdinEnabled: true
        onStarted: saved = ""

        stdout: SplitParser {
            onRead: line => {
                if (line.startsWith("saved "))
                    scrollProc.saved = line.slice(6)
                else
                    root.scrollFrames = parseInt(line) || root.scrollFrames
            }
        }

        onExited: code => {
            root.scrolling = false
            if (code !== 0 || !saved)
                return
            Quickshell.execDetached(root.sh(`wl-copy -t image/png < '${saved}'`))
            root.notify("Screenshot saved", saved.replace(root.home, "~"), saved)
        }
    }

    Process {
        id: colorProc

        stdout: StdioCollector {
            onStreamFinished: root.takeColour(text)
        }

        stderr: StdioCollector {
            onStreamFinished: root.takeColour(text)
        }
    }
}
