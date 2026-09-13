import QtQuick
import Quickshell
import "../../../config"
import ".."

Provider {
    id: root

    name: "web"
    label: "Web"
    prefix: "!"
    weight: 1

    readonly property var urlLike: /^(https?:\/\/\S+|www\.\S+|[\w-]+(\.[\w-]+)+(\/\S*)?)$/

    function open(url) {
        Quickshell.execDetached(["xdg-open", url])
    }

    function searchUrl(bang, term) {
        const b = SpotlightConfig.bangs[bang] ?? SpotlightConfig.bangs[SpotlightConfig.defaultBang]
        return b.url.replace("%s", encodeURIComponent(term))
    }

    function parse(text) {
        const m = text.match(/^!(\S*)\s*([\s\S]*)$/)
        return m ? { bang: m[1].toLowerCase(), term: m[2].trim() } : null
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
                section: "Search engines",
                icon: b.icon,
                title: b.name,
                subtitle: "!" + key,
                score: s,
                complete: "!" + key + " "
            })
        }
        return out
    }

    function search(text) {
        const trimmed = text.trim()
        if (!trimmed) {
            root.results = []
            return
        }

        const parsed = root.parse(trimmed)
        if (parsed) {
            const b = SpotlightConfig.bangs[parsed.bang]
            if (b && parsed.term) {
                root.results = [{
                    key: "web:" + parsed.bang,
                    kind: "web",
                    section: "Search engines",
                    icon: b.icon,
                    title: parsed.term,
                    subtitle: "Search " + b.name,
                    score: 1.5,
                    activate: () => root.open(root.searchUrl(parsed.bang, parsed.term))
                }]
            } else {
                root.results = root.bangSuggestions(parsed.bang)
            }
            return
        }

        const out = []

        if (root.urlLike.test(trimmed)) {
            const url = trimmed.startsWith("http") ? trimmed : "https://" + trimmed
            out.push({
                key: "url",
                kind: "web",
                section: "Web",
                icon: "link",
                title: trimmed,
                subtitle: "Open in browser",
                score: 1.4,
                activate: () => root.open(url)
            })
        }

        const def = SpotlightConfig.bangs[SpotlightConfig.defaultBang]
        out.push({
            key: "websearch",
            kind: "web",
            section: "Web",
            icon: def.icon,
            title: trimmed,
            subtitle: "Search " + def.name,
            score: 0.01,
            fallback: true,
            activate: () => root.open(root.searchUrl(SpotlightConfig.defaultBang, trimmed))
        })

        root.results = out
    }
}
