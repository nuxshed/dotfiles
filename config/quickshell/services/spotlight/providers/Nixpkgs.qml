import QtQuick
import Quickshell
import Quickshell.Io
import "../../../config"
import ".."

Provider {
    id: root

    name: "nixpkgs"
    label: "Nixpkgs"
    prefix: "nix"
    icon: "widgets"
    keywords: ["packages", "nixpkgs", "install", "nix search"]
    mixed: false
    weight: 1

    readonly property string index: SpotlightConfig.home + "/.cache/spotlight/nixpkgs.tsv"
    readonly property string bin: SpotlightConfig.home + "/dotfiles/bin/"
    readonly property string flake: "nixpkgs/nixpkgs-unstable"

    property string pending: ""
    property bool building: false
    property real age: -1
    property real checked: 0

    function terminal(args) {
        Quickshell.execDetached([SpotlightConfig.terminal, "-e"].concat(args))
    }

    function status() {
        if (root.age >= 0)
            return []
        return [{
            key: "nix:index",
            kind: "nixpkgs",
            section: "Nixpkgs",
            icon: "find_in_page",
            title: root.building ? "Building nixpkgs-unstable index…" : "Nixpkgs index missing",
            subtitle: root.building ? "Takes about a minute, results appear when done" : "Press Enter to build it",
            score: 2,
            pinned: true,
            activate: root.building ? null : () => root.build()
        }]
    }

    function build() {
        if (root.building)
            return
        root.building = true
        builder.running = true
    }

    function search(text) {
        root.pending = text.trim()
        debounce.stop()
        if (Date.now() - root.checked > 60000) {
            root.checked = Date.now()
            checker.running = true
        }
        if (root.pending.length < 2) {
            root.results = root.status()
            return
        }
        debounce.restart()
    }

    function accept(output) {
        const out = root.status()
        const lines = output.split("\n").filter(l => l.length > 0)
        lines.forEach((line, i) => {
            const [attr, version, description] = line.split("\t")
            const ref = root.flake + "#" + attr
            out.push({
                key: "nix:" + attr,
                kind: "nixpkgs",
                section: "Nixpkgs unstable",
                icon: "widgets",
                title: attr,
                subtitle: description || "No description",
                badge: version,
                score: 1 - i * 0.005,
                primary: "Open nix shell",
                activate: () => root.terminal(["nix", "shell", ref]),
                altActivate: () => root.terminal(["nix", "run", ref]),
                altHint: "nix run",
                altIcon: "play_arrow",
                copy: attr,
                copyTitle: "Copy attribute name",
                actions: [
                    { icon: "file_download", title: "Add to profile", run: () => root.terminal(["sh", "-c", 'nix profile add "$1"; echo; read -r -p "Press Enter to close" _', "sh", ref]) },
                    { icon: "open_in_new", title: "Open on search.nixos.org", run: () => Quickshell.execDetached(["xdg-open", "https://search.nixos.org/packages?channel=unstable&show=" + encodeURIComponent(attr) + "&query=" + encodeURIComponent(attr)]) },
                    { icon: "content_copy", title: "Copy nix shell command", run: () => Quickshell.execDetached(["wl-copy", "--", "nix shell " + ref]) }
                ],
                details: [
                    { label: "Version", value: version },
                    { label: "Attribute", value: attr }
                ]
            })
        })
        root.results = out
    }

    readonly property Timer debounce: Timer {
        interval: 180
        onTriggered: {
            finder.running = false
            finder.command = [root.bin + "qs-nixsearch", root.index].concat(root.pending.split(/\s+/))
            finder.running = true
        }
    }

    readonly property Process finder: Process {
        stdout: StdioCollector {
            onStreamFinished: root.accept(text)
        }
    }

    readonly property Process checker: Process {
        command: ["sh", "-c", 'f="$1"; [ -s "$f" ] && echo $(( $(date +%s) - $(stat -c %Y "$f") )) || echo -1', "sh", root.index]
        stdout: StdioCollector {
            onStreamFinished: {
                root.age = parseInt(text.trim())
                if (root.age < 0 || root.age > 3 * 86400)
                    root.build()
            }
        }
    }

    readonly property Process builder: Process {
        command: ["nice", "-n", "15", root.bin + "qs-nixindex"]
        onExited: {
            root.building = false
            root.checker.running = true
            if (root.pending.length >= 2)
                root.debounce.restart()
        }
    }
}
