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
    property string error: ""
    property bool inputless: false
    property bool echo: false
    property var callback: null
    property var cancelCallback: null

    function ask(config) {
        if (root.open)
            root.close()
        root.title = config.title ?? ""
        root.subtitle = config.subtitle ?? ""
        root.placeholder = config.placeholder ?? "Password"
        root.action = config.action ?? "Confirm"
        root.error = config.error ?? ""
        root.inputless = config.inputless ?? false
        root.echo = config.echo ?? false
        root.callback = config.onSubmit ?? null
        root.cancelCallback = config.onCancel ?? null
        root.open = true
    }

    function submit(value) {
        const pending = root.callback
        root.callback = null
        root.cancelCallback = null
        root.open = false
        if (pending)
            pending(value)
    }

    function close() {
        const cancel = root.cancelCallback
        root.open = false
        root.callback = null
        root.cancelCallback = null
        if (cancel)
            cancel()
    }
}
