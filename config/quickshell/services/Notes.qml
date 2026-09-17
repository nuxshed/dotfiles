pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io
import "../config"

Singleton {
    id: root

    property bool open: false
    property bool ready: false
    property var notes: []
    property int current: 0
    property int counter: 0

    readonly property var note: notes.find(n => n.id === current) ?? null
    readonly property string body: note?.body ?? ""

    function title(n): string {
        const line = (n.body ?? "").split("\n").find(l => l.trim().length > 0);
        return line ? line.trim().replace(/^(#+|[-*>]|\d+\.)\s+/, "").replace(/[*_`]/g, "").slice(0, 28) : "untitled";
    }

    function toggle(): void {
        root.open = !root.open;
    }

    function add(): void {
        const id = ++root.counter;
        root.notes = root.notes.concat([{ id, body: "" }]);
        root.current = id;
        root.open = true;
        root.persist();
    }

    function remove(id: int): void {
        const rest = root.notes.filter(n => n.id !== id);
        if (rest.length === 0) {
            root.add();
            root.notes = root.notes.filter(n => n.id !== id);
        } else if (root.current === id) {
            const i = root.notes.findIndex(n => n.id === id);
            root.current = rest[Math.min(i, rest.length - 1)].id;
        }
        root.notes = rest;
        root.persist();
    }

    function select(id: int): void {
        root.current = id;
        root.persist();
    }

    function cycle(step: int): void {
        const i = root.notes.findIndex(n => n.id === root.current);
        root.select(root.notes[(i + step + root.notes.length) % root.notes.length].id);
    }

    function setBody(text: string): void {
        if (!root.note || root.note.body === text)
            return;
        root.notes = root.notes.map(n => n.id === root.current ? { id: n.id, body: text } : n);
        root.persist();
    }

    function persist(): void {
        if (root.ready)
            save.restart();
    }

    function load(text: string): void {
        try {
            const data = JSON.parse(text);
            root.notes = data.notes ?? [];
            root.counter = data.counter ?? root.notes.reduce((m, n) => Math.max(m, n.id), 0);
            root.current = data.current ?? (root.notes[0]?.id ?? 0);
        } catch (e) {}
        root.ready = true;
        if (root.notes.length === 0)
            root.add();
    }

    FileView {
        id: file
        path: SpotlightConfig.stateDir + "/notes.json"
        printErrors: false
        onLoaded: root.load(text())
        onLoadFailed: root.load("")
    }

    Timer {
        id: save
        interval: 400
        onTriggered: file.setText(JSON.stringify({ notes: root.notes, current: root.current, counter: root.counter }))
    }
}
