import QtQuick
import Quickshell
import Quickshell.Io
import "../../../config"
import ".."

Provider {
    id: root

    name: "dictionary"
    label: "Dictionary"
    prefix: "d"
    icon: "book"
    keywords: ["define", "dictionary", "meaning", "word"]
    weight: 1

    property string word: ""
    property bool natural: false
    property var cache: ({})

    function target(text, scoped) {
        const q = text.trim().toLowerCase().replace(/[?.!]+$/, "")
        if (scoped)
            return /^[a-z][a-z' -]{0,40}$/.test(q) ? { word: q, natural: false } : null
        const m = q.match(/^(?:define|definition\s+of|meaning\s+of|what\s+does)\s+([a-z][a-z' -]{0,40}?)(?:\s+mean)?$/) ?? q.match(/^([a-z][a-z' -]{0,40}?)\s+(?:meaning|definition|defined)$/)
        return m ? { word: m[1].trim(), natural: true } : null
    }

    function plain(html) {
        return (html ?? "").replace(/<[^>]+>/g, "").replace(/&nbsp;/g, " ").replace(/&amp;/g, "&").replace(/&quot;/g, "\"").replace(/&#39;/g, "'").replace(/&lt;/g, "<").replace(/&gt;/g, ">").replace(/\s+/g, " ").trim()
    }

    function wiki(word) {
        return "https://en.wiktionary.org/wiki/" + encodeURIComponent(word.replace(/ /g, "_"))
    }

    function build(word, entries) {
        const out = []
        const limit = root.natural ? 3 : 8
        for (const e of entries) {
            for (const d of e.definitions ?? []) {
                const text = root.plain(d.definition)
                if (!text || out.length >= limit)
                    continue
                const example = root.plain((d.parsedExamples ?? [])[0]?.example ?? (d.examples ?? [])[0] ?? "")
                out.push({
                    key: "define:" + word,
                    kind: "dictionary",
                    view: "define",
                    section: "Dictionary",
                    icon: "book",
                    title: word,
                    pos: (e.partOfSpeech ?? "").toLowerCase(),
                    subtitle: text,
                    score: 1.5 - out.length * 0.01,
                    pinned: true,
                    primary: "Copy definition",
                    activate: () => Quickshell.execDetached(["wl-copy", "--", text]),
                    altActivate: () => Quickshell.execDetached(["xdg-open", root.wiki(word)]),
                    altHint: "Open in Wiktionary",
                    altIcon: "open_in_new",
                    actions: [{ icon: "content_copy", title: "Copy word", run: () => Quickshell.execDetached(["wl-copy", "--", word]) }],
                    details: example ? [{ label: "Example", value: example }] : []
                })
            }
        }
        return out
    }

    function search(text, scoped) {
        const t = root.target(text, !!scoped)
        debounce.stop()
        if (!t) {
            root.word = ""
            root.results = []
            return
        }
        root.word = t.word
        root.natural = t.natural
        if (root.cache[t.word]) {
            root.results = root.build(t.word, root.cache[t.word])
            return
        }
        root.results = []
        debounce.restart()
    }

    function accept(text, word) {
        if (word !== root.word)
            return
        let data
        try {
            data = JSON.parse(text)
        } catch (e) {
            return
        }
        const entries = data.en ?? []
        const next = Object.assign({}, root.cache)
        next[word] = entries
        root.cache = next
        root.results = root.build(word, entries)
    }

    readonly property Timer debounce: Timer {
        interval: 260
        onTriggered: {
            fetch.word = root.word
            fetch.running = false
            fetch.command = ["curl", "-fsS", "--max-time", "6", "-A", "quickshell-spotlight/1.0", "https://en.wiktionary.org/api/rest_v1/page/definition/" + encodeURIComponent(root.word)]
            fetch.running = true
        }
    }

    readonly property Process fetch: Process {
        property string word: ""

        stdout: StdioCollector {
            onStreamFinished: root.accept(text, root.fetch.word)
        }
    }
}
