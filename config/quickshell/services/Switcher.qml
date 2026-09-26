pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Hyprland

Singleton {
    id: root

    property bool open: false
    property bool shown: false
    property string mode: "windows"
    property int index: 0
    property var items: []
    property var history: []

    readonly property var current: items[index] ?? null

    function list(m: string): var {
        if (m === "workspaces")
            return HyprlandData.workspaces.filter(w => w.id > 0).sort((a, b) => root.rank(a) - root.rank(b) || a.id - b.id);
        return HyprlandData.windowList.filter(w => w.mapped && !w.hidden && w.workspace.id > 0).sort((a, b) => a.focusHistoryID - b.focusHistoryID);
    }

    function rank(ws: var): int {
        if (ws.id === Hyprland.focusedWorkspace?.id)
            return -1;
        const seen = root.history.indexOf(ws.id);
        if (seen >= 0)
            return seen;
        const recency = HyprlandData.windowList.filter(w => w.workspace.id === ws.id).map(w => w.focusHistoryID);
        return 1000 + (recency.length ? Math.min(...recency) : 1000);
    }

    function step(m: string, delta: int): void {
        if (root.open && root.mode === m) {
            root.move(delta);
            return;
        }
        const items = root.list(m);
        if (items.length === 0)
            return;
        const n = items.length;
        root.mode = m;
        root.items = items;
        root.index = n > 1 ? (delta + n) % n : 0;
        root.shown = false;
        root.open = true;
        reveal.restart();
    }

    function move(delta: int): void {
        const n = root.items.length;
        if (n > 0)
            root.index = (root.index + delta + n) % n;
        root.shown = true;
    }

    function select(i: int): void {
        root.index = i;
        root.commit();
    }

    function commit(): void {
        if (!root.open)
            return;
        const item = root.current;
        root.close();
        if (!item)
            return;
        if (root.mode === "workspaces")
            Hyprland.dispatch(`hl.dsp.focus({ workspace = ${item.id} })`);
        else
            Hyprland.dispatch(`hl.dsp.focus({ window = 'address:${item.address}' })`);
    }

    function cancel(): void {
        root.close();
    }

    function close(): void {
        reveal.stop();
        root.shown = false;
        root.open = false;
    }

    function moveWindow(address: string, workspace: int): void {
        Hyprland.dispatch(`hl.dsp.window.move({ workspace = ${workspace}, window = 'address:${address}' })`);
        HyprlandData.updateAll();
    }

    function toplevel(address: string): var {
        const key = address.replace(/^0x/, "");
        return Hyprland.toplevels.values.find(t => t.address === key)?.wayland ?? null;
    }

    function icon(cls: string): string {
        return DesktopEntries.heuristicLookup(cls)?.icon ?? "";
    }

    Timer {
        id: reveal
        interval: 160
        onTriggered: root.shown = true
    }

    Connections {
        target: Hyprland

        function onFocusedWorkspaceChanged() {
            const id = Hyprland.focusedWorkspace?.id ?? 0;
            if (id > 0)
                root.history = [id].concat(root.history.filter(h => h !== id));
        }
    }

    Connections {
        target: HyprlandData
        enabled: root.open

        function onWorkspacesChanged() {
            if (root.mode !== "workspaces")
                return;
            const items = root.list("workspaces");
            if (items.map(w => w.id).join() === root.items.map(w => w.id).join())
                return;
            root.items = items;
            root.index = Math.min(root.index, items.length - 1);
        }
    }
}
