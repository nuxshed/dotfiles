pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io

Singleton {
    id: root

    readonly property string dir: Quickshell.cachePath("files-thumbs")
    readonly property int size: 256

    property var known: ({})
    property var queued: ({})
    property var pending: []
    property int version: 0

    function kindFor(entry: var): string {
        if (entry.isDir)
            return "";
        const ext = entry.suffix.toLowerCase();
        if (["png", "jpg", "jpeg", "gif", "webp", "bmp", "avif", "heic", "tiff", "tif", "svg"].includes(ext))
            return "image";
        if (["mp4", "mkv", "webm", "mov", "avi", "m4v"].includes(ext))
            return "video";
        if (ext === "pdf")
            return "pdf";
        return "";
    }

    function keyFor(entry: var): string {
        return Qt.md5(`${entry.path}:${entry.mtime}:${entry.size}`) + ".png";
    }

    function lookup(entry: var): string {
        const kind = root.kindFor(entry);
        if (kind.length === 0)
            return "";
        const key = root.keyFor(entry);
        if (root.known[key])
            return root.dir + "/" + key;
        if (!root.queued[key]) {
            root.queued[key] = true;
            root.pending.push([entry.path, root.dir + "/" + key, kind]);
            flush.restart();
        }
        return "";
    }

    function run(): void {
        if (worker.running || root.pending.length === 0)
            return;
        const batch = root.pending.slice(0, 24);
        root.pending = root.pending.slice(24);
        worker.batch = batch.map(b => b[1]);
        const args = [];
        for (const b of batch)
            args.push(b[0], b[1], b[2]);
        worker.command = ["sh", "-c", root.script, "sh", root.dir, String(root.size)].concat(args);
        worker.running = true;
    }

    function finish(): void {
        const next = Object.assign({}, root.known);
        const q = Object.assign({}, root.queued);
        for (const out of worker.batch) {
            const key = out.slice(out.lastIndexOf("/") + 1);
            if (worker.produced[key])
                next[key] = true;
            delete q[key];
        }
        root.known = next;
        root.queued = q;
        root.version++;
        worker.produced = {};
        root.run();
    }

    readonly property string script: `
dir="$1"; size="$2"; shift 2
mkdir -p "$dir"
while [ $# -ge 3 ]; do
  src="$1"; out="$2"; kind="$3"; shift 3
  tmp="$out.tmp.png"
  case "$kind" in
    image) magick "$src[0]" -auto-orient -thumbnail "\${size}x\${size}>" -background none "$tmp" ;;
    video) ffmpeg -nostdin -loglevel error -y -ss 1 -i "$src" -frames:v 1 -vf "scale='min($size,iw)':-2" "$tmp" ;;
    pdf) pdftoppm -png -f 1 -l 1 -scale-to "$size" -singlefile "$src" "$out.tmp" ;;
  esac 2>/dev/null
  if [ -s "$tmp" ]; then mv -f "$tmp" "$out" && printf '%s\\n' "$out"; else rm -f "$tmp"; fi
done
`

    Process {
        id: worker

        property var batch: []
        property var produced: ({})

        stdout: SplitParser {
            onRead: line => {
                if (line.length > 0)
                    worker.produced[line.slice(line.lastIndexOf("/") + 1)] = true;
            }
        }

        onExited: root.finish()
    }

    Process {
        id: scan

        running: true
        command: ["sh", "-c", 'mkdir -p "$1" && ls -1 "$1"', "sh", root.dir]

        stdout: StdioCollector {
            onStreamFinished: {
                const map = {};
                for (const name of text.split("\n"))
                    if (name.endsWith(".png"))
                        map[name] = true;
                root.known = map;
                root.version++;
            }
        }
    }

    Timer {
        id: flush
        interval: 60
        onTriggered: root.run()
    }
}
