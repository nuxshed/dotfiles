pragma Singleton

import QtQuick
import Quickshell
import "../config"

Singleton {
    id: root

    property bool open: false
    property string tab: "theme"

    function show(name: string): void {
        root.tab = name === "wallpaper" ? "wallpaper" : "theme";
        root.open = true;
    }

    function toggle(name: string): void {
        if (root.open && root.tab === name)
            root.close();
        else
            root.show(name);
    }

    function close(): void {
        Colors.preview = "";
        Wallpapers.stopPreview();
        root.open = false;
    }
}
