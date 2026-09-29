import QtQuick
import Quickshell
import Quickshell.Io
import "../../components"

Scope {
    id: root

    property bool shouldShowOsd: false
    property real brightnessLevel: 0

    Process {
        id: brightnessProcess
        command: ["brightnessctl", "get"]
        running: true

        stdout: StdioCollector {
            onStreamFinished: {
                var currentBrightness = parseFloat(this.text.trim())
                if (currentBrightness !== root.brightnessLevel) {
                    root.brightnessLevel = currentBrightness
                    root.shouldShowOsd = true
                    hideTimer.restart()
                }
            }
        }
    }

    Process {
        id: maxBrightnessProcess
        command: ["brightnessctl", "max"]
        running: true

        stdout: StdioCollector {
            onStreamFinished: {
                root.maxBrightness = parseFloat(this.text.trim())
            }
        }
    }

    property real maxBrightness: 100

    Timer {
        id: brightnessTimer
        interval: 100
        running: true
        repeat: true
        onTriggered: brightnessProcess.running = true
    }

    Timer {
        id: hideTimer
        interval: 2000
        onTriggered: root.shouldShowOsd = false
    }

    LazyLoader {
        active: root.shouldShowOsd

        OsdCard {
            icon: "brightness_6"
            value: root.brightnessLevel / root.maxBrightness
        }
    }
}
