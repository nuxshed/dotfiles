import QtQuick
import Quickshell
import Quickshell.Io
import "../../../config"
import ".."

Provider {
    id: root

    name: "vault"
    label: "Bitwarden"
    prefix: "pw"
    icon: "vpn_key"
    keywords: ["passwords", "password", "vault", "bitwarden", "logins", "2fa", "totp"]
    mixed: false
    weight: 1

    property string state: "unknown"
    property var items: []
    property real probed: 0
    property string error: ""

    readonly property string loginScript: `
printf '\\033[1mBitwarden login for rbw\\033[0m\\n\\n'
printf 'Bitwarden needs new-device verification, which rbw does with your personal API key.\\n'
printf 'Get it at vault.bitwarden.com → Settings → Security → Keys → View API key.\\n'
printf 'rbw will ask for client_id and client_secret, then your master password.\\n\\n'
if rbw register && rbw login && rbw sync; then
  printf '\\nDone.\\n'; qs ipc call spotlight scope pw >/dev/null 2>&1 &
  sleep 1
else
  printf '\\nLogin failed. Check ~/.local/share/rbw/agent.err for details.\\n'; read -r -p 'Press Enter to close' _
fi`
    property string pending: ""
    property bool active: false

    readonly property string forget: 'sleep 0.5; line=$(cliphist list 2>/dev/null | head -n 1); [ -n "$line" ] && [ "$(printf "%s" "$line" | cliphist decode 2>/dev/null)" = "$s" ] && printf "%s" "$line" | cliphist delete; sleep 45; [ "$(wl-paste -n 2>/dev/null)" = "$s" ] && wl-copy --clear'

    function secret(cmd, id) {
        Quickshell.execDetached(["bash", "-c", `s=$(${cmd} "$1") || exit 1; printf "%s" "$s" | wl-copy; ${root.forget}`, "bash", id])
    }

    function typeIn(id) {
        Quickshell.execDetached(["bash", "-c", 'sleep 0.3; rbw get "$1" | tr -d "\\n" | wtype -', "bash", id])
    }

    function terminal(script) {
        Quickshell.execDetached([SpotlightConfig.terminal, "-e", "bash", "-c", script])
    }

    function refresh() {
        root.probed = Date.now()
        if (!probe.running)
            probe.running = true
    }

    function status() {
        const s = {
            missing: { icon: "vpn_key", title: "rbw is not installed", subtitle: "Rebuild home-manager (rbw was added to modules/desktop/spotlight.nix)", activate: null },
            setup: { icon: "vpn_key", title: "Set up Bitwarden", subtitle: "Configure your email and log in with rbw", activate: () => root.terminal('read -r -p "Bitwarden email: " e && rbw config set email "$e" && rbw config set pinentry "$HOME/dotfiles/bin/pinentry-qs" && rbw config set lock_timeout 43200 && rbw login && rbw sync; echo; read -r -p "Press Enter to close" _') },
            login: { icon: "vpn_key", title: "Log in to Bitwarden", subtitle: "Registers this device with your API key, then logs in", activate: () => root.terminal(root.loginScript) },
            locked: { icon: "lock", title: "Unlock vault", subtitle: root.error || "Opens the rbw password prompt", activate: () => { root.error = ""; unlocker.running = true } },
            unknown: { icon: "vpn_key", title: "Checking vault…", subtitle: "", activate: null }
        }[root.state]
        if (!s)
            return []
        return [Object.assign({ key: "vault:" + root.state, kind: "vault", section: "Bitwarden", score: 2, pinned: true }, s)]
    }

    function search(text, scoped) {
        root.pending = text.trim()
        root.active = true
        if (Date.now() - root.probed > (root.state === "ok" ? 30000 : 3000))
            root.refresh()
        if (root.state !== "ok") {
            root.results = root.status()
            return
        }

        const q = root.pending.toLowerCase()
        const out = []
        for (const e of root.items) {
            const s = q ? Fuzzy.best(q, [e.name, e.user, e.folder ? e.folder + " " + e.name : ""]) : 0.5
            if (s < 0.4)
                continue
            out.push({
                key: "vault:" + e.id,
                kind: "vault",
                section: e.folder || "Bitwarden",
                icon: "vpn_key",
                title: e.name,
                subtitle: e.user || "No username",
                badge: e.folder,
                score: s,
                primary: "Copy password",
                activate: () => root.secret("rbw get", e.id),
                altActivate: () => root.typeIn(e.id),
                altHint: "Type password",
                altIcon: "keyboard",
                actions: [
                    { icon: "content_copy", title: "Copy username", run: () => Quickshell.execDetached(["wl-copy", "--", e.user]) },
                    { icon: "timer", title: "Copy TOTP code", run: () => root.secret("rbw code", e.id) }
                ]
            })
        }
        out.sort((a, b) => b.score - a.score)
        out.push({
            key: "vault:sync",
            kind: "vault",
            section: "Vault",
            icon: "autorenew",
            title: "Sync vault",
            subtitle: root.items.length + " items",
            score: 0.01,
            pinned: true,
            activate: () => Quickshell.execDetached(["rbw", "sync"]),
            actions: [{ icon: "lock", title: "Lock vault", run: () => Quickshell.execDetached(["rbw", "lock"]) }]
        })
        root.results = out.slice(0, 50)
    }

    function clear() {
        root.active = false
        root.pending = ""
        root.results = []
    }

    function accept(text) {
        const lines = text.split("\n")
        root.state = (lines.shift() ?? "").trim() || "missing"
        if (root.state === "ok") {
            root.items = lines.filter(l => l.length > 0).map(l => {
                const [id, name, user, folder] = l.split("\t")
                return { id: id, name: name ?? "", user: user ?? "", folder: folder ?? "" }
            })
        }
        if (root.active)
            root.search(root.pending, true)
    }

    readonly property Process probe: Process {
        command: ["bash", "-c", `
command -v rbw >/dev/null || { echo missing; exit; }
rbw config show 2>/dev/null | grep -q '"email": *"' || { echo setup; exit; }
ls "\${XDG_CACHE_HOME:-$HOME/.cache}"/rbw/*.json >/dev/null 2>&1 || { echo login; exit; }
rbw unlocked >/dev/null 2>&1 || { echo locked; exit; }
echo ok
rbw list --fields id,name,user,folder 2>/dev/null`]
        stdout: StdioCollector {
            onStreamFinished: root.accept(text)
        }
    }

    readonly property Process unlocker: Process {
        command: ["rbw", "unlock"]
        stderr: StdioCollector {
            id: unlockErr
        }
        onExited: (code) => {
            root.probed = 0
            root.refresh()
            if (code === 0) {
                Quickshell.execDetached(["qs", "ipc", "call", "spotlight", "scope", "pw"])
            } else {
                root.error = unlockErr.text.trim().split("\n").pop() || "Unlock failed"
                Quickshell.execDetached(["notify-send", "-a", "Bitwarden", "-i", "dialog-password", "Unlock failed", root.error])
            }
        }
    }
}
