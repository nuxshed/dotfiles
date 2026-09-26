pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io

Singleton {
    id: root

    readonly property string device: "intel_backlight"

    property real value: 0
    property real pending: -1
    property bool watching: false

    function set(v: real): void {
        root.value = Math.max(0.01, Math.min(1, v));
        root.pending = root.value;
        if (!apply.running)
            apply.start();
    }

    Timer {
        interval: 500
        running: root.watching
        repeat: true
        triggeredOnStart: true
        onTriggered: if (root.pending < 0) read.running = true
    }

    Timer {
        id: apply
        interval: 40
        onTriggered: {
            Quickshell.execDetached(["brightnessctl", "-d", root.device, "-q", "s", Math.round(root.pending * 100) + "%"]);
            root.pending = -1;
        }
    }

    Process {
        id: read
        command: ["brightnessctl", "-d", root.device, "-m"]
        stdout: StdioCollector {
            onStreamFinished: {
                const f = text.trim().split(",");
                if (f.length >= 5 && root.pending < 0)
                    root.value = parseInt(f[2]) / parseInt(f[4]);
            }
        }
    }
}
