pragma Singleton

import Quickshell
import Quickshell.Io
import Quickshell.Services.Pam

Singleton {
    id: root

    property bool locked: false
    property string buffer: ""
    property string error: ""
    property string sessionPath: ""

    readonly property bool busy: pam.active

    function lock(): void {
        if (root.locked)
            return;
        root.buffer = "";
        root.error = "";
        root.locked = true;
    }

    function type(text: string): void {
        if (root.busy)
            return;
        root.error = "";
        root.buffer += text;
    }

    function erase(all: bool): void {
        if (root.busy)
            return;
        root.error = "";
        root.buffer = all ? "" : root.buffer.slice(0, -1);
    }

    function submit(): void {
        if (root.busy || root.buffer.length === 0)
            return;
        root.error = "";
        pam.start();
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
                root.locked = false;
                Quickshell.execDetached(["loginctl", "unlock-session"]);
            } else {
                root.error = result === PamResult.Failed ? "incorrect password" : "authentication error";
            }
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
        running: root.sessionPath.length > 0
        command: ["dbus-monitor", "--system", "type='signal',interface='org.freedesktop.login1.Session'"]

        stdout: SplitParser {
            onRead: line => {
                if (line.includes(`path=${root.sessionPath};`) && line.includes("member=Lock"))
                    root.lock();
            }
        }
    }
}
