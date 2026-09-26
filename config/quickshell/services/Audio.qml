pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Services.Pipewire

Singleton {
    id: root

    readonly property PwNode sink: Pipewire.defaultAudioSink
    readonly property PwNode source: Pipewire.defaultAudioSource
    readonly property var nodes: Pipewire.nodes.values.filter(n => n.audio)
    readonly property var sinks: nodes.filter(n => !n.isStream && n.isSink)
    readonly property var sources: nodes.filter(n => !n.isStream && !n.isSink)
    readonly property var streams: nodes.filter(n => n.isStream && n.isSink)

    function name(node: PwNode): string {
        if (!node)
            return "";
        if (node.isStream)
            return root.appEntry(node)?.name || node.properties["application.name"] || node.description || node.name;
        return node.description || node.nickname || node.name;
    }

    function detail(node: PwNode): string {
        const media = node?.properties["media.name"] ?? "";
        return media.toLowerCase() === root.name(node).toLowerCase() ? "" : media;
    }

    function appEntry(node: PwNode): var {
        const props = node?.properties ?? {};
        const binary = (props["application.process.binary"] ?? "").replace(/^\./, "").replace(/-wrapped$/, "");
        const window = HyprlandData.windowList.find(w => w.pid == props["application.process.id"]);
        for (const name of [props["application.icon-name"], binary, props["application.name"], window?.class]) {
            const entry = name ? DesktopEntries.heuristicLookup(name) : null;
            if (entry)
                return entry;
        }
        return null;
    }

    function appIcon(node: PwNode): string {
        return node?.properties["application.icon-name"] || root.appEntry(node)?.icon || "";
    }

    function deviceIcon(node: PwNode): string {
        if (!node)
            return "volume_off";
        const n = (node.name + " " + node.description).toLowerCase();
        if (!node.isSink)
            return "mic";
        if (n.includes("bluez"))
            return "bluetooth_audio";
        if (n.includes("head"))
            return "headset";
        if (n.includes("hdmi"))
            return "tv";
        return "speaker";
    }

    function volumeIcon(node: PwNode): string {
        const a = node?.audio;
        if (!a || a.muted || a.volume <= 0)
            return node && !node.isSink && !node.isStream ? "mic_off" : "volume_off";
        if (!node.isSink && !node.isStream)
            return "mic";
        return a.volume < 0.5 ? "volume_down" : "volume_up";
    }

    function setVolume(node: PwNode, value: real): void {
        if (node?.audio) {
            node.audio.muted = false;
            node.audio.volume = value;
        }
    }

    function toggleMute(node: PwNode): void {
        if (node?.audio)
            node.audio.muted = !node.audio.muted;
    }

    function setDefault(node: PwNode): void {
        if (node.isSink)
            Pipewire.preferredDefaultAudioSink = node;
        else
            Pipewire.preferredDefaultAudioSource = node;
    }

    PwObjectTracker {
        objects: root.nodes
    }
}
