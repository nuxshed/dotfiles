import QtQuick
import Quickshell
import Quickshell.Services.Pipewire
import "../../components"
import "../../services"

Scope {
    id: root

    PwObjectTracker {
        objects: [ Pipewire.defaultAudioSink ]
    }

    Connections {
        target: Pipewire.defaultAudioSink?.audio

        function onVolumeChanged() {
            root.shouldShowOsd = true;
            hideTimer.restart();
        }
    }

    property bool shouldShowOsd: false

    Timer {
        id: hideTimer
        interval: 2000
        onTriggered: root.shouldShowOsd = false
    }

    LazyLoader {
        active: root.shouldShowOsd

        OsdCard {
            icon: Audio.volumeIcon(Pipewire.defaultAudioSink)
            value: Pipewire.defaultAudioSink?.audio.volume ?? 0
            muted: Pipewire.defaultAudioSink?.audio.muted ?? false
        }
    }
}
