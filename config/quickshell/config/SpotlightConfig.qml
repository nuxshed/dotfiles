pragma Singleton

import QtQuick
import Quickshell

QtObject {
    readonly property string home: Quickshell.env("HOME")
    readonly property string indexDb: home + "/.cache/spotlight/index.db"
    readonly property string stateDir: home + "/.local/state/quickshell"
    readonly property string frecencyFile: stateDir + "/spotlight.json"

    readonly property int maxMixed: 8
    readonly property int maxScoped: 40
    readonly property int maxFilesMixed: 3
    readonly property int fileDebounce: 90

    readonly property string terminal: "wezterm"
    readonly property string browser: "firefox"
    readonly property string fileManager: "thunar"

    readonly property string defaultBang: "ddg"

    readonly property var bangs: ({
        ddg: { name: "DuckDuckGo", icon: "travel_explore", url: "https://duckduckgo.com/?q=%s" },
        g: { name: "Google", icon: "search", url: "https://www.google.com/search?q=%s" },
        yt: { name: "YouTube", icon: "smart_display", url: "https://www.youtube.com/results?search_query=%s" },
        gh: { name: "GitHub", icon: "code", url: "https://github.com/search?q=%s" },
        nix: { name: "Nix Packages", icon: "deployed_code", url: "https://search.nixos.org/packages?query=%s" },
        opt: { name: "NixOS Options", icon: "tune", url: "https://search.nixos.org/options?query=%s" },
        hm: { name: "Home Manager", icon: "home", url: "https://home-manager-options.extranix.com/?query=%s" },
        w: { name: "Wikipedia", icon: "menu_book", url: "https://en.wikipedia.org/w/index.php?search=%s" },
        aw: { name: "Arch Wiki", icon: "description", url: "https://wiki.archlinux.org/index.php?search=%s" },
        so: { name: "Stack Overflow", icon: "quiz", url: "https://stackoverflow.com/search?q=%s" },
        mdn: { name: "MDN", icon: "public", url: "https://developer.mozilla.org/en-US/search?q=%s" },
        r: { name: "Reddit", icon: "forum", url: "https://www.reddit.com/search/?q=%s" },
        crate: { name: "crates.io", icon: "inventory_2", url: "https://crates.io/search?q=%s" },
        qs: { name: "Quickshell Docs", icon: "terminal", url: "https://quickshell.org/docs/v0.2.1/search/?q=%s" }
    })
}
