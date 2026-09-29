pragma Singleton

import QtQuick
import Quickshell

Singleton {
    id: root

    property bool open: false
    property string page: "appearance"

    function show(name: string): void {
        if (name.length > 0)
            root.page = name;
        root.open = true;
    }

    function toggle(): void {
        root.open = !root.open;
    }
}
