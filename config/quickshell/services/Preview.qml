pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io
import "../config"

Singleton {
    id: root

    readonly property string home: Quickshell.env("HOME")

    property bool open: false
    property string path: ""
    property string title: ""
    property int rotation: 0
    property real zoom: 1
    property bool fit: true
    property string tool: "none"
    property color stroke: Colors.red
    property var shapes: []
    property var draft: null
    property bool editing: false
    property bool cropping: false
    property var cropRect: null
    property var cropSel: null
    property bool dirty: false
    property string notice: ""
    property int generation: 0
    property string pendingMode: ""

    signal flattenRequested()
    signal fitRequested()
    signal saveAsRequested(string tmp, string dir, string name, int w, int h)

    function openFile(p: string): void {
        if (p.length === 0)
            return;
        root.reset();
        root.path = p.startsWith("file://") ? decodeURIComponent(p.slice(7)) : p;
        root.title = root.basename(root.path);
        root.generation++;
        root.open = true;
    }

    function reset(): void {
        root.rotation = 0;
        root.zoom = 1;
        root.fit = true;
        root.tool = "none";
        root.shapes = [];
        root.draft = null;
        root.editing = false;
        root.cropping = false;
        root.cropRect = null;
        root.cropSel = null;
        root.dirty = false;
        root.notice = "";
        root.pendingMode = "";
    }

    function close(): void {
        root.open = false;
        root.path = "";
        root.reset();
    }

    function setTool(t: string): void {
        root.cropping = false;
        root.tool = root.tool === t ? "none" : t;
    }

    function addShape(shape: var): void {
        root.shapes = root.shapes.concat([shape]);
        root.dirty = true;
    }

    function undo(): void {
        if (root.shapes.length > 0) {
            root.shapes = root.shapes.slice(0, -1);
            root.dirty = true;
        }
    }

    function rotate(delta: int): void {
        root.rotation = ((root.rotation + delta) % 360 + 360) % 360;
        root.dirty = true;
        root.fitRequested();
    }

    function zoomBy(factor: real): void {
        root.fit = false;
        root.zoom = Math.max(0.1, Math.min(8, root.zoom * factor));
    }

    function zoomActual(): void {
        root.fit = false;
        root.zoom = 1;
    }

    function zoomFit(): void {
        root.fit = true;
        root.fitRequested();
    }

    function startCrop(): void {
        root.tool = "none";
        root.cropSel = null;
        root.cropping = true;
    }

    function applyCrop(): void {
        if (root.cropSel) {
            const o = root.cropRect ?? { x: 0, y: 0, w: 1, h: 1 };
            const s = root.cropSel;
            root.cropRect = { x: o.x + s.x * o.w, y: o.y + s.y * o.h, w: s.w * o.w, h: s.h * o.h };
            root.dirty = true;
        }
        root.cropSel = null;
        root.cropping = false;
    }

    function resetCrop(): void {
        root.cropRect = null;
        root.cropSel = null;
        root.dirty = true;
    }

    function cancelCrop(): void {
        root.cropSel = null;
        root.cropping = false;
    }

    function flash(text: string): void {
        root.notice = text;
        noticeTimer.restart();
    }

    // save() overwrites the original; saveCopy() opens a save picker; copyToClipboard() pipes to wl-copy.
    function save(): void {
        root.pendingMode = "save";
        root.flattenRequested();
    }

    function saveCopy(): void {
        root.pendingMode = "saveas";
        root.flattenRequested();
    }

    function copyToClipboard(): void {
        root.pendingMode = "clip";
        root.flattenRequested();
    }

    // Called by the panel once it has rendered the edited image to `tmp` (a PNG).
    // Normalises the DPR-scaled grab down to exact pixels, then routes it.
    readonly property string fitCmd: 'magick "$1" -resize "${3}x${4}!"'

    function delivered(tmp: string, w: int, h: int): void {
        const mode = root.pendingMode;
        root.pendingMode = "";
        if (mode === "save") {
            const dest = root.path.toLowerCase().endsWith(".png") ? root.path : root.pngSibling(root.path);
            Quickshell.execDetached(["sh", "-c", `${root.fitCmd} "$2" && rm -f "$1"`, "sh", tmp, dest, String(w), String(h)]);
            if (dest !== root.path) {
                root.path = dest;
                root.title = root.basename(dest);
            }
            root.dirty = false;
            root.flash("Saved");
        } else if (mode === "clip") {
            Quickshell.execDetached(["sh", "-c", `${root.fitCmd} png:- | wl-copy -t image/png; rm -f "$1"`, "sh", tmp, "", String(w), String(h)]);
            root.flash("Copied to clipboard");
        } else if (mode === "saveas") {
            const dot = root.title.lastIndexOf(".");
            const stem = dot > 0 ? root.title.slice(0, dot) : root.title;
            root.saveAsRequested(tmp, root.parentOf(root.path), `${stem} (edited).png`, w, h);
        }
    }

    function savedAs(tmp: string, dest: string, w: int, h: int): void {
        const out = dest.toLowerCase().endsWith(".png") ? dest : dest + ".png";
        Quickshell.execDetached(["sh", "-c", `mkdir -p "$(dirname "$2")" && ${root.fitCmd} "$2" && rm -f "$1"`, "sh", tmp, out, String(w), String(h)]);
        root.dirty = false;
        root.flash("Saved to " + root.pretty(out));
    }

    function pngSibling(p: string): string {
        const dot = p.lastIndexOf(".");
        return (dot > 0 ? p.slice(0, dot) : p) + ".png";
    }

    function basename(p: string): string {
        const i = p.lastIndexOf("/");
        return i < 0 ? p : p.slice(i + 1);
    }

    function parentOf(p: string): string {
        const i = p.lastIndexOf("/");
        return i <= 0 ? root.home : p.slice(0, i);
    }

    function pretty(p: string): string {
        return p.startsWith(root.home) ? "~" + p.slice(root.home.length) : p;
    }

    Timer {
        id: noticeTimer
        interval: 2600
        onTriggered: root.notice = ""
    }
}
