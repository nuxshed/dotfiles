pragma Singleton
pragma ComponentBehavior: Bound

import QtQuick
import Quickshell
import Quickshell.Io
import Quickshell.Hyprland

Singleton {
    id: root

    readonly property int barCount: 24
    readonly property string home: Quickshell.env("HOME")
    readonly property string videoDir: home + "/Videos/Recordings"
    readonly property string audioDir: home + "/Audio/Recordings"

    property string kind: ""
    property bool active: kind !== ""
    property int elapsed: 0
    property string outPath: ""
    property bool discarding: false
    property real level: 0
    property var levels: new Array(barCount).fill(0)
    property var history: []

    signal finished(string path, string kind, var peaks)

    function sh(script) {
        return ["bash", "-c", script]
    }

    function pushLevel(value) {
        const next = levels.slice(1)
        next.push(value)
        levels = next
        level = value
        history.push(value)
    }

    function resetLevels() {
        levels = new Array(barCount).fill(0)
        level = 0
    }

    function resample(source, count) {
        if (!source || source.length === 0)
            return new Array(count).fill(0)
        const out = []
        const step = source.length / count
        for (let i = 0; i < count; i++) {
            const start = Math.floor(i * step)
            const end = Math.max(start + 1, Math.floor((i + 1) * step))
            let peak = 0
            for (let j = start; j < end && j < source.length; j++)
                peak = Math.max(peak, source[j])
            out.push(peak)
        }
        return out
    }

    function startScreen(geometry) {
        if (active)
            return
        const monitor = Hyprland.focusedMonitor?.name ?? ""
        const path = `${videoDir}/recording_${Capture.stamp()}.mkv`
        const region = geometry ? `-g '${geometry}'` : ""

        kind = "screen"
        outPath = path
        elapsed = 0
        discarding = false
        history = []
        resetLevels()

        recordProc.exec(sh(`mkdir -p '${videoDir}' && src="$(pactl get-default-sink).monitor" && exec wf-recorder -o '${monitor}' --pixel-format yuv420p ${region} --audio="$src" -f '${path}'`))
    }

    function startVoice() {
        if (active)
            return
        const path = `${audioDir}/voice_${Capture.stamp()}.opus`

        kind = "voice"
        outPath = path
        elapsed = 0
        discarding = false
        history = []
        resetLevels()

        recordProc.exec(sh(`mkdir -p '${audioDir}' && exec ffmpeg -hide_banner -loglevel error -f pulse -i default -ac 1 -af astats=metadata=1:reset=1,ametadata=print:key=lavfi.astats.Overall.RMS_level:file=/dev/stderr -c:a libopus -b:a 96k -y '${path}'`))
    }

    function stop() {
        if (!active)
            return
        recordProc.signal(2)
    }

    function discard() {
        if (!active)
            return
        discarding = true
        stop()
    }

    Timer {
        running: root.active
        interval: 1000
        repeat: true
        onTriggered: root.elapsed++
    }

    Timer {
        running: root.active
        interval: 60
        repeat: true
        onTriggered: if (root.level > 0) root.level = Math.max(0, root.level - 0.08)
    }

    component LevelParser: SplitParser {
        onRead: data => {
            const match = data.match(/RMS_level=(-?[\d.]+|-inf)/)
            if (!match)
                return
            const db = match[1] === "-inf" ? -100 : parseFloat(match[1])
            root.pushLevel(Math.max(0, Math.min(1, (db + 55) / 55)))
        }
    }

    Process {
        id: recordProc

        stderr: LevelParser {}

        onExited: {
            const path = root.outPath
            const wasKind = root.kind
            const wasDiscarded = root.discarding
            const peaks = root.history.slice()

            root.kind = ""
            root.outPath = ""
            root.elapsed = 0
            root.discarding = false
            root.history = []
            root.resetLevels()

            if (wasDiscarded) {
                Quickshell.execDetached(root.sh(`rm -f '${path}'`))
                return
            }
            root.finished(path, wasKind, peaks)
        }
    }

}
