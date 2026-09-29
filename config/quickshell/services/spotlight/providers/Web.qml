import QtQuick
import Quickshell
import Quickshell.Io
import "../../../config"
import ".."

Provider {
    id: root

    name: "web"
    label: "Web"
    prefix: "!"
    weight: 1

    readonly property var urlLike: /^(https?:\/\/\S+|www\.\S+|[\w-]+(\.[\w-]+)+(\/\S*)?)$/
    readonly property var alternates: ["g", "yt", "gh", "w", "r"]

    property var base: []
    property var suggested: []
    property string suggestBang: ""
    property string suggestTerm: ""

    function open(url) {
        Quickshell.execDetached(["xdg-open", url])
    }

    function searchUrl(bang, term) {
        const b = SpotlightConfig.bangs[bang] ?? SpotlightConfig.bangs[Settings.searchEngine]
        return b.url.replace("%s", encodeURIComponent(term))
    }

    function parse(text) {
        const m = text.match(/^!(\S*)\s*([\s\S]*)$/)
        return m ? { bang: m[1].toLowerCase(), term: m[2].trim() } : null
    }

    function clear() {
        suggest.stop()
        fetch.running = false
        root.suggestTerm = ""
        root.base = []
        root.suggested = []
        root.results = []
    }

    function publish() {
        root.results = root.base.concat(root.suggested)
    }

    function alternatesFor(bang, term) {
        return root.alternates.filter(k => k !== bang && SpotlightConfig.bangs[k]).map(k => ({
            icon: "search",
            title: "Search " + SpotlightConfig.bangs[k].name,
            run: () => root.open(root.searchUrl(k, term))
        }))
    }

    function searchItem(bang, term, score, section) {
        const b = SpotlightConfig.bangs[bang]
        return {
            key: "web:" + bang,
            kind: "web",
            view: "web",
            section: section,
            icon: b.icon,
            favicon: b.domain,
            title: term,
            subtitle: "Search " + b.name,
            badge: "!" + bang,
            score: score,
            copy: root.searchUrl(bang, term),
            copyTitle: "Copy search URL",
            activate: () => root.open(root.searchUrl(bang, term)),
            actions: root.alternatesFor(bang, term)
        }
    }

    function bangSuggestions(partial) {
        const out = []
        for (const key in SpotlightConfig.bangs) {
            const b = SpotlightConfig.bangs[key]
            const s = Fuzzy.best(partial, [key, b.name])
            if (s < 0)
                continue

            out.push({
                key: "bang:" + key,
                kind: "web",
                view: "web",
                section: "Search engines",
                icon: b.icon,
                favicon: b.domain,
                title: b.name,
                subtitle: b.domain,
                badge: "!" + key,
                score: s,
                complete: "!" + key + " "
            })
        }
        return out
    }

    function search(text) {
        const trimmed = text.trim()
        root.suggested = []
        suggest.stop()

        if (!trimmed) {
            root.base = []
            root.publish()
            return
        }

        const parsed = root.parse(trimmed)
        if (parsed) {
            const b = SpotlightConfig.bangs[parsed.bang]
            if (b && parsed.term) {
                root.base = [root.searchItem(parsed.bang, parsed.term, 1.5, "Search engines")]
                root.suggestBang = parsed.bang
                root.suggestTerm = parsed.term
                suggest.restart()
            } else {
                root.base = root.bangSuggestions(parsed.bang)
            }
            root.publish()
            return
        }

        const out = []

        if (root.urlLike.test(trimmed)) {
            const url = trimmed.startsWith("http") ? trimmed : "https://" + trimmed
            const host = url.replace(/^https?:\/\//, "").split(/[/?#]/)[0]
            out.push({
                key: "url",
                kind: "web",
                view: "web",
                section: "Web",
                icon: "link",
                title: trimmed,
                subtitle: "Open in browser",
                badge: host,
                score: 1.4,
                copy: url,
                copyTitle: "Copy URL",
                activate: () => root.open(url)
            })
        }

        const fallback = root.searchItem(Settings.searchEngine, trimmed, 0.01, "Web")
        fallback.fallback = true
        out.push(fallback)

        root.base = out
        root.publish()
    }

    function acceptSuggestions(text) {
        let data
        try {
            data = JSON.parse(text)
        } catch (e) {
            return
        }
        if (!Array.isArray(data) || data[0] !== root.suggestTerm)
            return

        const bang = root.suggestBang
        const b = SpotlightConfig.bangs[bang]
        const lower = root.suggestTerm.toLowerCase()
        root.suggested = (data[1] ?? []).filter(s => s.toLowerCase() !== lower).slice(0, 5).map((s, i) => ({
            key: "suggest:" + bang + ":" + s,
            kind: "web",
            view: "web",
            section: "Suggestions",
            icon: "search",
            title: s,
            subtitle: "Search " + b.name,
            score: 1.2 - i * 0.01,
            complete: "!" + bang + " " + s,
            activate: () => root.open(root.searchUrl(bang, s)),
            actions: root.alternatesFor(bang, s)
        }))
        root.publish()
    }

    readonly property Timer suggest: Timer {
        interval: SpotlightConfig.suggestDebounce
        onTriggered: {
            fetch.running = false
            fetch.command = ["curl", "-fsS", "--max-time", "3", "https://duckduckgo.com/ac/?type=list&q=" + encodeURIComponent(root.suggestTerm)]
            fetch.running = true
        }
    }

    readonly property Process fetch: Process {
        stdout: StdioCollector {
            onStreamFinished: root.acceptSuggestions(text)
        }
    }
}
