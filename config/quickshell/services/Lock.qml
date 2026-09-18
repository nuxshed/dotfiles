pragma Singleton

import Quickshell
import Quickshell.Io
import Quickshell.Services.Pam
import QtQuick

Singleton {
    id: root

    property bool locked: false
    property string buffer: ""
    property string error: ""
    property string sessionPath: ""
    property string mode: "clock"
    property bool dimmed: false
    property bool unlocking: false
    property bool secure: false
    property bool sleeping: false
    property bool autoBounce: false

    readonly property bool busy: pam.active
    readonly property int idleTimeout: 120000

    function lock(): void {
        if (root.locked)
            return;
        root.buffer = "";
        root.error = "";
        root.locked = true;
        root.touch();
    }

    function touch(): void {
        root.dimmed = false;
        if (root.autoBounce) {
            root.autoBounce = false;
            root.mode = "clock";
        }
        if (root.mode === "clock")
            idle.restart();
        else
            idle.stop();
    }

    function setMode(name: string): void {
        root.autoBounce = false;
        root.mode = name === "bounce" ? "bounce" : "clock";
        root.touch();
    }

    function toggleMode(): void {
        root.setMode(root.mode === "clock" ? "bounce" : "clock");
    }

    function type(text: string): void {
        root.error = "";
        root.buffer += text;
    }

    function erase(all: bool): void {
        root.error = "";
        root.buffer = all ? "" : root.buffer.slice(0, -1);
    }

    function submit(): void {
        if (root.busy || root.buffer.length === 0)
            return;
        root.error = "";
        pam.start();
    }

    function prepareForSleep(sleep: bool): void {
        root.sleeping = sleep;
        if (sleep) {
            root.lock();
            root.dimmed = true;
            if (root.secure)
                inhibit.running = false;
        } else {
            inhibit.running = true;
            root.touch();
        }
    }

    onSecureChanged: {
        if (root.secure && root.sleeping)
            inhibit.running = false;
    }

    PamContext {
        id: pam

        config: "swaylock"
        configDirectory: "/etc/pam.d"

        onPamMessage: {
            if (pam.responseRequired)
                pam.respond(root.buffer);
        }

        onCompleted: result => {
            root.buffer = "";

            if (result === PamResult.Success) {
                root.error = "";
                root.unlocking = true;
                idle.stop();
                release.start();
            } else {
                root.error = result === PamResult.Failed ? "incorrect password" : "authentication error";
            }
        }
    }

    Process {
        id: inhibit

        running: true
        command: ["systemd-inhibit", "--what=sleep", "--who=quickshell", "--why=lock screen", "--mode=delay", "sleep", "infinity"]
    }

    Timer {
        id: idle

        interval: root.idleTimeout
        onTriggered: {
            root.autoBounce = true;
            root.mode = "bounce";
        }
    }

    Timer {
        id: release

        interval: 350
        onTriggered: {
            root.locked = false;
            root.unlocking = false;
            root.dimmed = false;
            Quickshell.execDetached(["loginctl", "unlock-session"]);
        }
    }

    Process {
        running: true
        command: ["busctl", "--system", "call", "org.freedesktop.login1", "/org/freedesktop/login1", "org.freedesktop.login1.Manager", "GetSession", "s", Quickshell.env("XDG_SESSION_ID") ?? ""]

        stdout: StdioCollector {
            onStreamFinished: {
                const found = text.match(/"([^"]+)"/);
                if (found)
                    root.sessionPath = found[1];
            }
        }
    }

    Process {
        id: monitor

        running: root.sessionPath.length > 0
        command: ["dbus-monitor", "--system", "type='signal',interface='org.freedesktop.login1.Session'", "type='signal',interface='org.freedesktop.login1.Manager',member='PrepareForSleep'"]

        property bool sleepSignal: false

        stdout: SplitParser {
            onRead: line => {
                if (line.includes(`path=${root.sessionPath};`) && line.includes("member=Lock"))
                    root.lock();
                else if (line.includes("member=PrepareForSleep"))
                    monitor.sleepSignal = true;
                else if (monitor.sleepSignal && line.includes("boolean")) {
                    monitor.sleepSignal = false;
                    root.prepareForSleep(line.includes("true"));
                }
            }
        }
    }
}
