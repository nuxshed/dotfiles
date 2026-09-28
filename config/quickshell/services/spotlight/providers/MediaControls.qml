import QtQuick
import Quickshell
import "../../../services"
import ".."

Provider {
    id: root

    name: "media"
    label: "Media"
    prefix: "mu"
    icon: "music_note"
    keywords: ["music", "media", "player", "playback"]
    weight: 1.05
    cap: 3

    readonly property var commands: [
        { id: "toggle", keywords: ["play", "pause", "resume", "play pause", "toggle music"], run: () => Mpris.togglePlayPause() },
        { id: "next", title: "Next track", icon: "skip_next", keywords: ["next", "skip", "next song", "next track"], run: () => Mpris.next() },
        { id: "previous", title: "Previous track", icon: "skip_previous", keywords: ["previous", "prev", "back", "previous song", "last song"], run: () => Mpris.previous() },
        { id: "stop", title: "Stop playback", icon: "stop", keywords: ["stop", "stop music"], run: () => Mpris.stop() },
        { id: "shuffle", keywords: ["shuffle"], run: () => { if (Mpris.active?.shuffleSupported) Mpris.active.shuffle = !Mpris.active.shuffle } },
        { id: "now", keywords: ["now playing", "np", "song", "music", "track", "what's playing"], run: () => Mpris.togglePlayPause() }
    ]

    function track() {
        return Mpris.trackTitle ? Mpris.trackTitle + (Mpris.trackArtist ? " · " + Mpris.trackArtist : "") : (Mpris.active?.identity ?? "")
    }

    function describe(c) {
        const playing = Mpris.isPlaying
        if (c.id === "toggle")
            return { title: playing ? "Pause" : "Play", icon: playing ? "pause" : "play_arrow" }
        if (c.id === "shuffle")
            return { title: Mpris.active?.shuffle ? "Turn off shuffle" : "Turn on shuffle", icon: "shuffle" }
        if (c.id === "now")
            return { title: Mpris.trackTitle || "Nothing playing", icon: "music_note" }
        return { title: c.title, icon: c.icon }
    }

    function search(text, scoped) {
        const q = text.trim().toLowerCase()
        if (!Mpris.hasActivePlayer || (!q && !scoped)) {
            root.results = []
            return
        }

        const out = []
        for (const c of root.commands) {
            if (c.id === "shuffle" && !Mpris.active?.shuffleSupported)
                continue
            const s = q ? Fuzzy.best(q, c.keywords) : (c.id === "now" ? 0.6 : 0.5)
            if (s < (scoped ? 0.4 : 0.85))
                continue
            const d = root.describe(c)
            const now = c.id === "now"
            out.push({
                key: "media:" + c.id,
                kind: "media",
                section: "Media",
                icon: d.icon,
                art: now ? String(Mpris.artworkUrl) : "",
                title: d.title,
                subtitle: now ? (Mpris.trackArtist || Mpris.active?.identity || "") : root.track(),
                badge: Mpris.active?.identity ?? "",
                live: Mpris.isPlaying && now,
                score: s + (now ? 0.05 : 0),
                primary: now ? (Mpris.isPlaying ? "Pause" : "Play") : d.title,
                activate: c.run,
                actions: now ? [
                    { icon: "skip_next", title: "Next track", run: () => Mpris.next() },
                    { icon: "skip_previous", title: "Previous track", run: () => Mpris.previous() }
                ] : []
            })
        }
        root.results = out
    }
}
