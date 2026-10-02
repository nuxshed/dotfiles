pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io

Singleton {
    id: root

    property bool camera: false
    property bool mic: false

    readonly property bool recording: Recorder.active
    readonly property bool pomodoro: Pomodoro.active
    readonly property bool micOnly: mic && Recorder.kind !== "voice"
    readonly property bool timers: Timers.any
    readonly property bool any: recording || pomodoro || timers

    Process {
        id: probe
        command: [Quickshell.env("HOME") + "/dotfiles/bin/qs-activities"]
        stdout: StdioCollector {
            onStreamFinished: {
                const parts = text.trim().split(" ");
                root.camera = parts[0] === "1";
                root.mic = parts[1] === "1";
            }
        }
    }

    Timer {
        interval: 2000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: probe.running = true
    }
}
