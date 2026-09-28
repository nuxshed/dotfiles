pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io
import "../../config"

Singleton {
    id: root

    property var entries: ({})
    property var pendingKeys: []
    property bool ready: false

    readonly property real halfLife: 14 * 24 * 3600 * 1000

    function boost(key) {
        const e = root.entries[key]
        if (!e)
            return 1

        const age = Date.now() - (e.t ?? 0)
        const decay = Math.pow(0.5, age / root.halfLife)
        return 1 + Math.min(1.1, Math.log(1 + (e.n ?? 0)) * 0.45) * decay
    }

    function ranked() {
        const now = Date.now()
        return Object.keys(root.entries)
            .map(k => [k, (root.entries[k].n ?? 0) * Math.pow(0.5, (now - (root.entries[k].t ?? 0)) / root.halfLife)])
            .sort((a, b) => b[1] - a[1])
            .map(e => e[0])
    }

    function record(key) {
        if (!key)
            return

        const e = root.entries[key] ?? { n: 0, t: 0 }
        const next = {}
        for (const k in root.entries)
            next[k] = root.entries[k]
        next[key] = { n: (e.n ?? 0) + 1, t: Date.now() }
        root.entries = next

        if (!root.ready) {
            const p = root.pendingKeys.slice()
            p.push(key)
            root.pendingKeys = p
        }

        saveTimer.restart()
    }

    function load(text) {
        if (root.ready)
            return

        let stored = ({})
        try {
            stored = text ? JSON.parse(text) : ({})
        } catch (e) {
            stored = ({})
        }

        const merged = {}
        for (const k in stored)
            merged[k] = stored[k]

        for (const key of root.pendingKeys) {
            const e = merged[key] ?? { n: 0, t: 0 }
            merged[key] = { n: (e.n ?? 0) + 1, t: Date.now() }
        }

        root.entries = merged
        root.pendingKeys = []
        root.ready = true
    }

    readonly property Timer saveTimer: Timer {
        interval: 1500
        onTriggered: file.setText(JSON.stringify(root.entries))
    }

    readonly property Process mkdir: Process {
        running: true
        command: ["mkdir", "-p", SpotlightConfig.stateDir]
    }

    readonly property FileView file: FileView {
        path: SpotlightConfig.frecencyFile
        printErrors: false
        onLoaded: root.load(text())
        onLoadFailed: root.load("")
    }
}
