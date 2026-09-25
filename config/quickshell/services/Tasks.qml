pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io
import "../config"

Singleton {
    id: root

    property var tasks: []
    property int counter: 0
    property bool ready: false

    readonly property var open: tasks.filter(t => !t.done)
    readonly property var done: tasks.filter(t => t.done)

    function add(text: string): void {
        const body = text.trim();
        if (body.length === 0)
            return;
        root.tasks = root.tasks.concat([{ id: ++root.counter, text: body, done: false }]);
        root.persist();
    }

    function toggle(id: int): void {
        root.tasks = root.tasks.map(t => t.id === id ? { id: t.id, text: t.text, done: !t.done } : t);
        root.persist();
    }

    function remove(id: int): void {
        root.tasks = root.tasks.filter(t => t.id !== id);
        root.persist();
    }

    function clearDone(): void {
        root.tasks = root.open;
        root.persist();
    }

    function persist(): void {
        if (root.ready)
            save.restart();
    }

    function load(text: string): void {
        try {
            const data = JSON.parse(text);
            root.tasks = data.tasks ?? [];
            root.counter = data.counter ?? 0;
        } catch (e) {}
        root.ready = true;
    }

    FileView {
        id: file
        path: SpotlightConfig.stateDir + "/tasks.json"
        printErrors: false
        onLoaded: root.load(text())
        onLoadFailed: root.load("")
    }

    Timer {
        id: save
        interval: 300
        onTriggered: file.setText(JSON.stringify({ tasks: root.tasks, counter: root.counter }))
    }
}
