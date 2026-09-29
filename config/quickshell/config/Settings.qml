pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io

Singleton {
    id: root

    readonly property string home: Quickshell.env("HOME")
    readonly property var keys: ["theme", "wallpaper", "wallpaperDir", "wallpaperFit", "clock24", "toolbar", "controlCenter", "notifTimeout", "lockStyle", "weatherLocation", "searchEngine"]

    property bool ready: false

    property string theme: "graphite"
    property string wallpaper: home + "/Pictures/Wallpapers/leaves-dark.jpg"
    property string wallpaperDir: home + "/Pictures/Wallpapers"
    property string wallpaperFit: "fill"
    property bool clock24: true
    property bool toolbar: true
    property bool controlCenter: true
    property int notifTimeout: 5
    property string lockStyle: "clock"
    property string weatherLocation: ""
    property string searchEngine: "ddg"

    function set(key: string, value: var): void {
        if (root[key] === value)
            return;
        root[key] = value;
        if (root.ready)
            save.restart();
    }

    Timer {
        id: save
        interval: 300
        onTriggered: {
            const out = {};
            for (const k of root.keys)
                out[k] = root[k];
            file.setText(JSON.stringify(out, null, 2));
        }
    }

    FileView {
        id: file
        path: root.home + "/.local/state/quickshell/settings.json"
        printErrors: false
        onLoaded: {
            try {
                const data = JSON.parse(text());
                for (const k of root.keys)
                    if (data[k] !== undefined && typeof data[k] === typeof root[k])
                        root[k] = data[k];
            } catch (e) {}
            root.ready = true;
            if (!Themes.list.some(t => t.id === root.theme))
                root.set("theme", Themes.list[0].id);
        }
        onLoadFailed: root.ready = true
    }
}
