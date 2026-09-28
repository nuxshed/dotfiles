pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io
import "../../config"

Singleton {
    id: root

    property var known: ({})

    function path(domain: string): string {
        return domain && root.known[domain] ? "file://" + SpotlightConfig.faviconDir + "/" + domain + ".png" : "";
    }

    readonly property var domains: {
        const out = [];
        for (const k in SpotlightConfig.bangs)
            if (SpotlightConfig.bangs[k].domain)
                out.push(SpotlightConfig.bangs[k].domain);
        for (const l of SpotlightConfig.quicklinks)
            if (l.domain && !out.includes(l.domain))
                out.push(l.domain);
        return out;
    }

    Process {
        running: true
        command: ["sh", "-c", `
dir="$1"; shift
mkdir -p "$dir"
for d in "$@"; do
  f="$dir/$d.png"
  if [ ! -s "$f" ]; then
    curl -fsSL --max-time 8 -o "$f.tmp" "https://www.google.com/s2/favicons?domain=$d&sz=64" && [ -s "$f.tmp" ] && mv -f "$f.tmp" "$f"
    rm -f "$f.tmp"
  fi
  [ -s "$f" ] && printf '%s\\n' "$d"
done
`, "sh", SpotlightConfig.faviconDir].concat(root.domains)

        stdout: StdioCollector {
            onStreamFinished: {
                const map = {};
                for (const d of text.split("\n"))
                    if (d.length > 0)
                        map[d] = true;
                root.known = map;
            }
        }
    }
}
