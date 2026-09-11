pragma Singleton

import QtQuick
import Quickshell

/**
 * Global password prompt state, rendered by PasswordDialog.
 */
Singleton {
    id: root

    property bool open: false
    property string title: ""
    property string subtitle: ""
    property string placeholder: ""
    property string action: "Confirm"
    property var callback: null

    function ask(config) {
        root.title = config.title ?? ""
        root.subtitle = config.subtitle ?? ""
        root.placeholder = config.placeholder ?? "Password"
        root.action = config.action ?? "Confirm"
        root.callback = config.onSubmit ?? null
        root.open = true
    }

    function submit(value) {
        const pending = root.callback
        root.close()
        if (pending)
            pending(value)
    }

    function close() {
        root.open = false
        root.callback = null
    }
}
