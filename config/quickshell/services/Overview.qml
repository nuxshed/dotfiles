pragma Singleton

import QtQuick
import Quickshell

Singleton {
    id: root

    property bool open: false
    property string tab: "calendar"
    property date selected: new Date()

    readonly property var tabs: [
        { id: "calendar", icon: "today", name: "Calendar" },
        { id: "focus", icon: "timer", name: "Focus" },
        { id: "tasks", icon: "assignment", name: "Tasks" },
        { id: "screentime", icon: "data_usage", name: "Screen time" }
    ]

    function toggle(): void {
        root.open = !root.open;
    }

    function cycle(step: int): void {
        const i = root.tabs.findIndex(t => t.id === root.tab);
        root.tab = root.tabs[(i + step + root.tabs.length) % root.tabs.length].id;
    }

    function show(name: string): void {
        if (name && root.tabs.some(t => t.id === name))
            root.tab = name;
        root.open = true;
    }
}
