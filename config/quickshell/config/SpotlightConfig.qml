pragma Singleton

import QtQuick
import Quickshell

QtObject {
    readonly property string home: Quickshell.env("HOME")
    readonly property string indexDb: home + "/.cache/spotlight/index.db"
    readonly property string stateDir: home + "/.local/state/quickshell"
    readonly property string frecencyFile: stateDir + "/spotlight.json"
    readonly property string faviconDir: home + "/.cache/quickshell/favicons"

    readonly property int maxMixed: 8
    readonly property int maxScoped: 40
    readonly property int maxFilesMixed: 3
    readonly property int fileDebounce: 90
    readonly property int suggestDebounce: 140

    readonly property string terminal: "wezterm"
    readonly property string browser: "firefox"
    readonly property string fileManager: "thunar"

    readonly property string homeCurrency: "INR"
    readonly property var currencies: ["USD", "EUR", "GBP"]
    readonly property int homeApps: 6
    readonly property var defaultApps: ["zen-beta", "org.wezfurlong.wezterm", "obsidian", "spotify", "code", "thunar"]

    readonly property var focusApps: ["discord", "vesktop", "beeper", "telegram", "slack", "steam", "whatsapp"]

    readonly property var snippets: [
        { keyword: "shrug", name: "Shrug", text: "¯\\_(ツ)_/¯" },
        { keyword: "flip", name: "Table flip", text: "(╯°□°)╯︵ ┻━┻" },
        { keyword: "lenny", name: "Lenny face", text: "( ͡° ͜ʖ ͡°)" },
        { keyword: "date", name: "Today's date", text: "{date}" },
        { keyword: "time", name: "Current time", text: "{time}" },
        { keyword: "now", name: "Date and time", text: "{datetime}" },
        { keyword: "iso", name: "ISO timestamp", text: "{iso}" },
        { keyword: "uuid", name: "Random UUID", text: "{uuid}" },
        { keyword: "gh", name: "GitHub profile link", text: "https://github.com/nuxshed" },
        { keyword: "dots", name: "Dotfiles repo link", text: "https://github.com/nuxshed/dotfiles" },
        { keyword: "mdlink", name: "Markdown link to clipboard URL", text: "[]({clipboard})" },
        { keyword: "lgtm", name: "LGTM", text: "LGTM, thanks!" },
        { keyword: "arrow", name: "Arrow", text: "→" },
        { keyword: "check", name: "Check mark", text: "✓" }
    ]

    readonly property var quicklinks: [
        { keyword: "repo", name: "GitHub repository", url: "https://github.com/%s", domain: "github.com", hint: "owner/name" },
        { keyword: "gh", name: "My GitHub", url: "https://github.com/nuxshed", domain: "github.com" },
        { keyword: "dots", name: "Dotfiles on GitHub", url: "https://github.com/nuxshed/dotfiles", domain: "github.com" },
        { keyword: "prs", name: "GitHub pull requests", url: "https://github.com/pulls", domain: "github.com" },
        { keyword: "issues", name: "GitHub issues", url: "https://github.com/issues", domain: "github.com" },
        { keyword: "notifs", name: "GitHub notifications", url: "https://github.com/notifications", domain: "github.com" },
        { keyword: "stars", name: "GitHub stars", url: "https://github.com/nuxshed?tab=stars", domain: "github.com" },
        { keyword: "nixpr", name: "Nixpkgs PR tracker", url: "https://nixpk.gs/pr-tracker.html?pr=%s", domain: "nixpk.gs", hint: "PR number" },
        { keyword: "nixissue", name: "Nixpkgs issues", url: "https://github.com/NixOS/nixpkgs/issues?q=%s", domain: "github.com", hint: "search" },
        { keyword: "hypr", name: "Hyprland wiki", url: "https://wiki.hypr.land/", domain: "wiki.hypr.land" },
        { keyword: "qsdocs", name: "Quickshell docs", url: "https://quickshell.org/docs/", domain: "quickshell.org" },
        { keyword: "lh", name: "localhost", url: "http://localhost:%s", icon: "computer", hint: "port" },
        { keyword: "claude", name: "Ask Claude", url: "https://claude.ai/new?q=%s", domain: "claude.ai", hint: "prompt" },
        { keyword: "mail", name: "Gmail", url: "https://mail.google.com", domain: "mail.google.com" },
        { keyword: "gcal", name: "Google Calendar", url: "https://calendar.google.com", domain: "calendar.google.com" }
    ]


    readonly property var bangs: ({
        ddg: { name: "DuckDuckGo", icon: "public", domain: "duckduckgo.com", url: "https://duckduckgo.com/?q=%s" },
        g: { name: "Google", icon: "search", domain: "google.com", url: "https://www.google.com/search?q=%s" },
        yt: { name: "YouTube", icon: "play_arrow", domain: "youtube.com", url: "https://www.youtube.com/results?search_query=%s" },
        gh: { name: "GitHub", icon: "code", domain: "github.com", url: "https://github.com/search?q=%s" },
        nix: { name: "Nix Packages", icon: "widgets", domain: "search.nixos.org", url: "https://search.nixos.org/packages?query=%s" },
        opt: { name: "NixOS Options", icon: "tune", domain: "nixos.org", url: "https://search.nixos.org/options?query=%s" },
        hm: { name: "Home Manager", icon: "home", domain: "home-manager-options.extranix.com", url: "https://home-manager-options.extranix.com/?query=%s" },
        w: { name: "Wikipedia", icon: "book", domain: "wikipedia.org", url: "https://en.wikipedia.org/w/index.php?search=%s" },
        aw: { name: "Arch Wiki", icon: "description", domain: "wiki.archlinux.org", url: "https://wiki.archlinux.org/index.php?search=%s" },
        so: { name: "Stack Overflow", icon: "info", domain: "stackoverflow.com", url: "https://stackoverflow.com/search?q=%s" },
        mdn: { name: "MDN", icon: "public", domain: "developer.mozilla.org", url: "https://developer.mozilla.org/en-US/search?q=%s" },
        r: { name: "Reddit", icon: "forum", domain: "reddit.com", url: "https://www.reddit.com/search/?q=%s" },
        crate: { name: "crates.io", icon: "archive", domain: "crates.io", url: "https://crates.io/search?q=%s" },
        qs: { name: "Quickshell Docs", icon: "code", domain: "quickshell.org", url: "https://quickshell.org/docs/v0.2.1/search/?q=%s" }
    })
}
