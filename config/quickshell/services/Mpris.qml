pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Services.Mpris

/**
 * Media player service using MPRIS.
 */
Singleton {
    id: root

    readonly property list<MprisPlayer> players: Mpris.players.values
    property MprisPlayer pinned: null
    readonly property MprisPlayer preferred: {
        const playing = p => p.playbackState === MprisPlaybackState.Playing;
        const spotify = players.filter(p => `${p.identity} ${p.desktopEntry}`.toLowerCase().includes("spotify"));
        return spotify.find(playing) ?? spotify[0] ?? players.find(playing) ?? players[0] ?? null;
    }
    readonly property MprisPlayer active: pinned && players.includes(pinned) ? pinned : preferred
    readonly property int activePlayerIndex: players.indexOf(active)
    readonly property bool hasMultiplePlayers: players.length > 1
    
    readonly property bool hasActivePlayer: active !== null
    readonly property string trackTitle: active?.trackTitle ?? ""
    readonly property string trackArtist: active?.trackArtist ?? ""
    readonly property string trackAlbum: active?.trackAlbum ?? ""
    readonly property url artworkUrl: active?.trackArtUrl ?? ""
    readonly property bool isPlaying: active?.playbackState === MprisPlaybackState.Playing
    readonly property bool canPlay: active?.canPlay ?? false
    readonly property bool canPause: active?.canPause ?? false
    readonly property bool canGoNext: active?.canGoNext ?? false
    readonly property bool canGoPrevious: active?.canGoPrevious ?? false
    readonly property real length: active?.length ?? 0
    
    property real position: 0
    readonly property real progress: length > 0 ? position / length : 0
    
    // Update position from player when it changes
    Connections {
        target: active
        function onPositionChanged() {
            root.position = root.active?.position ?? 0
        }
    }
    
    // Poll position while playing since not all players send position updates
    Timer {
        interval: 1000
        running: root.isPlaying
        repeat: true
        onTriggered: {
            if (root.active) {
                root.position = root.active.position ?? 0
            }
        }
    }
    
    // Initialize position when active player changes
    onActiveChanged: {
        position = active?.position ?? 0
    }
    
    function setActivePlayer(index) {
        if (index >= 0 && index < players.length)
            pinned = players[index]
    }
    
    function play() {
        if (active?.canPlay)
            active.play()
    }
    
    function pause() {
        if (active?.canPause)
            active.pause()
    }
    
    function togglePlayPause() {
        if (active?.canTogglePlaying)
            active.togglePlaying()
    }
    
    function next() {
        if (active?.canGoNext)
            active.next()
    }
    
    function previous() {
        if (active?.canGoPrevious)
            active.previous()
    }
    
    function stop() {
        active?.stop()
    }
}
