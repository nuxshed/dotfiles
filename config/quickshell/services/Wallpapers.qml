pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io
import "../config"

Singleton {
    id: root

    property var list: []
    property string previewPath: ""
    readonly property string current: Settings.wallpaper
    readonly property string shown: previewPath.length > 0 ? previewPath : current
    readonly property int index: list.findIndex(w => w.path === current)

    function set(path: string): void {
        Settings.set("wallpaper", path);
        root.stopPreview();
    }

    function preview(path: string): void {
        root.previewPath = path === root.current ? "" : path;
        Colors.previewWallpaper = root.previewPath;
    }

    function stopPreview(): void {
        root.previewPath = "";
        Colors.previewWallpaper = "";
    }

    function step(delta: int): void {
        if (list.length === 0)
            return;
        const i = root.index < 0 ? 0 : (root.index + delta + list.length) % list.length;
        root.set(list[i].path);
    }

    function random(): void {
        if (list.length < 2)
            return;
        let i = root.index;
        while (i === root.index)
            i = Math.floor(Math.random() * list.length);
        root.set(list[i].path);
    }

    function refresh(): void {
        scan.command = ["find", "-L", Settings.wallpaperDir, "-maxdepth", "2", "-type", "f", "(", "-iname", "*.jpg", "-o", "-iname", "*.jpeg", "-o", "-iname", "*.png", "-o", "-iname", "*.webp", ")", "-printf", "%T@\\t%s\\t%p\\n"];
        scan.running = true;
    }

    Connections {
        target: Settings

        function onWallpaperDirChanged(): void {
            root.refresh();
        }
    }

    Component.onCompleted: refresh()

    Process {
        id: scan

        stdout: StdioCollector {
            onStreamFinished: {
                const out = [];
                for (const line of text.split("\n")) {
                    const f = line.split("\t");
                    const path = f[2];
                    if (!path)
                        continue;
                    const file = path.slice(path.lastIndexOf("/") + 1);
                    const dot = file.lastIndexOf(".");
                    out.push({
                        path: path,
                        name: file.slice(0, dot).replace(/[-_]+/g, " "),
                        suffix: file.slice(dot + 1),
                        mtime: f[0],
                        size: f[1],
                        isDir: false
                    });
                }
                out.sort((a, b) => a.name.localeCompare(b.name));
                root.list = out;
            }
        }
    }
}
