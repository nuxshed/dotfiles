pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io

Singleton {
    id: root

    readonly property string dir: Quickshell.env("HOME") + "/Pictures/Camera"
    readonly property var effects: ["Normal", "Sepia", "Black & White", "Thermal", "X-Ray", "Pop Art", "Comic", "Kaleidoscope", "Bulge", "Dent", "Twirl", "Squeeze", "Mirror", "Fish Eye", "Stretch", "Light Tunnel"]

    property bool open: false
    property int effect: 0
    property bool mirror: true
    property bool picking: false
    property int countdown: 0
    property bool flash: false
    property string notice: ""
    property var shots: []

    signal captureRequested(string path)

    function toggle(): void {
        root.open = !root.open;
    }

    function show(): void {
        root.open = true;
    }

    function close(): void {
        tick.stop();
        root.countdown = 0;
        root.picking = false;
        root.open = false;
    }

    function pick(i: int): void {
        root.effect = i;
        root.picking = false;
    }

    function shoot(): void {
        if (!root.open || root.countdown > 0)
            return;
        root.picking = false;
        root.countdown = 3;
        tick.start();
    }

    function cancel(): void {
        tick.stop();
        root.countdown = 0;
    }

    function stamp(): string {
        const d = new Date();
        const p = n => String(n).padStart(2, "0");
        return `${d.getFullYear()}${p(d.getMonth() + 1)}${p(d.getDate())}-${p(d.getHours())}${p(d.getMinutes())}${p(d.getSeconds())}`;
    }

    function capture(): void {
        root.flash = true;
        flashTimer.restart();
        root.captureRequested(`${root.dir}/booth_${root.stamp()}.png`);
    }

    function deliver(tmp: string, path: string, w: int, h: int): void {
        writer.command = ["sh", "-c", 'magick "$1" -resize "$3x$4!" "$2" && rm -f "$1" && printf "%s" "$2"', "sh", tmp, path, String(w), String(h)];
        writer.running = true;
    }

    function saved(path: string): void {
        root.shots = [path].concat(root.shots).slice(0, 24);
        root.notice = "Saved " + path.slice(path.lastIndexOf("/") + 1);
        noticeTimer.restart();
    }

    function refresh(): void {
        lister.running = true;
    }

    onOpenChanged: if (open) root.refresh()

    Timer {
        id: tick
        interval: 600
        repeat: true
        onTriggered: {
            root.countdown--;
            if (root.countdown <= 0) {
                tick.stop();
                root.capture();
            }
        }
    }

    Timer {
        id: flashTimer
        interval: 260
        onTriggered: root.flash = false
    }

    Timer {
        id: noticeTimer
        interval: 2500
        onTriggered: root.notice = ""
    }

    Process {
        id: writer
        stdout: StdioCollector {
            onStreamFinished: if (text.length > 0) root.saved(text)
        }
    }

    Process {
        id: lister
        command: ["sh", "-c", 'mkdir -p "$1" && cd "$1" && ls -t *.png *.jpg 2>/dev/null | head -n 24 | sed "s|^|$1/|"', "sh", root.dir]
        stdout: StdioCollector {
            onStreamFinished: root.shots = text.split("\n").filter(l => l.length > 0)
        }
    }
}
