pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io

/**
 * OpenVPN service backed by the systemd unit, authenticated with sudo.
 */
Singleton {
    id: root

    readonly property string unit: "openvpn-college.service"
    readonly property string name: "college"
    readonly property bool busy: actionProc.running
    property bool active: false
    property string error: ""
    property bool initialized: false

    onActiveChanged: {
        if (!root.initialized)
            return

        notifyProc.exec(["notify-send", "-a", "VPN",
            "-i", root.active ? "network-vpn" : "network-vpn-disconnected",
            root.active ? "VPN connected" : "VPN disconnected", root.name])
    }

    function check() {
        checkProc.running = true
    }

    function toggle(password) {
        root.error = ""
        actionProc.password = password
        actionProc.command = ["sudo", "-S", "-k", "-p", "", "systemctl", root.active ? "stop" : "start", root.unit]
        actionProc.running = true
    }

    Component.onCompleted: check()

    Process {
        id: checkProc

        command: ["systemctl", "is-active", root.unit]
        stdout: StdioCollector {
            onStreamFinished: {
                root.active = text.trim() === "active"
                root.initialized = true
            }
        }
    }

    Process {
        id: actionProc

        property string password: ""

        stdinEnabled: true

        onStarted: {
            write(actionProc.password + "\n")
            actionProc.password = ""
        }

        stderr: StdioCollector {
            onStreamFinished: {
                const message = text.trim()
                if (message.includes("incorrect password") || message.includes("Sorry, try again"))
                    root.error = "Wrong password"
                else if (message.length > 0)
                    root.error = message.split("\n")[0]
            }
        }

        onExited: root.check()
    }

    Process {
        id: notifyProc
    }

    Timer {
        interval: 5000
        running: true
        repeat: true
        onTriggered: root.check()
    }
}
