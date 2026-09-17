pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Io
import "../config"
import "spotlight"

Singleton {
    id: root

    readonly property string home: Quickshell.env("HOME")

    property bool open: false
    property bool picker: false
    property bool multiple: false
    property bool directory: false
    property bool save: false
    property string outFile: ""
    property string cwd: home
    property string query: ""
    property string fileName: ""
    property bool showHidden: false
    property string sortBy: "name"
    property bool sortDesc: false
    property bool loading: lister.running
    property string error: ""
    property bool pending: false
    property list<string> selected: []
    property list<string> backStack: []
    property list<string> forwardStack: []
    property var entries: []
    property var places: []
    property int generation: 0

    property bool searching: false
    property string searchQuery: ""
    property var searchResults: []
    property bool searchBusy: finder.running
    property var pinned: []
    property string view: "list"
    property string special: ""
    property var recentEntries: []
    property bool recentBusy: recentStat.running
    property var ownRecent: []
    property var tabs: [{ cwd: home, back: [], forward: [] }]
    property int tab: 0
    property var openWith: null
    property var props: null
    property var saveCallback: null
    property var dragPaths: []

    property var clip: ({ op: "", paths: [] })
    property var dialog: null
    property string notice: ""
    property bool busy: op.running
    property string folderSize: ""
    property string sizedPath: ""

    readonly property var visibleEntries: root.searching ? root.searchResults : root.special === "recent" ? root.recentEntries : root.entries

    readonly property var items: {
        if (root.searching)
            return root.searchResults;
        if (root.special === "recent") {
            const q = root.query.toLowerCase();
            return q.length === 0 ? root.recentEntries : root.recentEntries.filter(e => e.name.toLowerCase().includes(q));
        }
        const q = root.query.toLowerCase();
        const list = root.entries.filter(e => (root.showHidden || !e.hidden) && (q.length === 0 || e.name.toLowerCase().includes(q)));
        const dir = root.sortDesc ? -1 : 1;
        list.sort((a, b) => {
            if (a.isDir !== b.isDir)
                return a.isDir ? -1 : 1;
            let d = 0;
            if (root.sortBy === "modified")
                d = a.mtime - b.mtime;
            else if (root.sortBy === "size")
                d = a.size - b.size;
            return d !== 0 ? d * dir : root.naturalCompare(a.name, b.name) * dir;
        });
        return list;
    }

    readonly property var selectedEntries: root.visibleEntries.filter(e => root.selected.includes(e.path))

    readonly property string title: {
        if (!root.picker)
            return "Files";
        if (root.save)
            return "Save file";
        if (root.directory)
            return root.multiple ? "Select folders" : "Select folder";
        return root.multiple ? "Open files" : "Open file";
    }

    readonly property string action: root.save ? (root.saveExists ? "Replace" : "Save") : root.directory ? "Select" : "Open"

    readonly property bool saveExists: root.save && root.entries.some(e => !e.isDir && e.name === root.fileName.trim())

    readonly property bool canAccept: {
        if (!root.picker)
            return false;
        if (root.save)
            return root.validName(root.fileName);
        if (root.directory)
            return root.selected.length === 0 || root.selectedEntries.every(e => e.isDir);
        const sel = root.selectedEntries;
        return sel.length > 0 && (sel.every(e => !e.isDir) || (sel.length === 1 && sel[0].isDir));
    }

    readonly property string status: {
        if (root.notice.length > 0)
            return root.notice;
        if (root.save)
            return root.saveExists ? "A file with this name already exists and will be replaced" : "";
        if (root.selected.length === 0) {
            if (root.picker)
                return root.directory ? "Current folder will be selected" : "";
            const n = root.items.length;
            return n === 1 ? "1 item" : `${n} items`;
        }
        if (root.selected.length === 1) {
            const e = root.selectedEntries[0];
            if (!e)
                return "";
            if (e.isDir)
                return root.sizedPath === e.path && root.folderSize.length > 0 ? `${e.name} · ${root.folderSize}` : e.name;
            return `${e.name} · ${root.formatSize(e.size)}`;
        }
        const total = root.selectedEntries.reduce((s, e) => s + (e.isDir ? 0 : e.size), 0);
        return `${root.selected.length} items selected · ${root.formatSize(total)}`;
    }

    function request(multiple: string, directory: string, save: string, path: string, out: string): void {
        if (root.open)
            root.finish([]);

        root.saveCallback = null;
        root.picker = true;
        root.multiple = multiple === "1";
        root.directory = directory === "1";
        root.save = save === "1";
        root.outFile = out;

        let start = path;
        let name = "";
        if (root.save && path.length > 0 && !path.endsWith("/")) {
            name = root.basename(path);
            start = root.parentOf(path);
        }
        if (start.length === 0 || !start.startsWith("/"))
            start = root.home;

        root.begin(start, name);
    }

    function show(): void {
        root.browse(root.cwd);
    }

    function browse(path: string): void {
        if (root.open && root.picker)
            root.finish([]);
        root.saveCallback = null;
        root.picker = false;
        root.multiple = true;
        root.directory = false;
        root.save = false;
        root.outFile = "";
        root.begin(path.length > 0 && path.startsWith("/") ? path : root.home, "");
    }

    function begin(start: string, name: string): void {
        root.backStack = [];
        root.forwardStack = [];
        root.selected = [];
        root.query = "";
        root.dialog = null;
        root.openWith = null;
        root.props = null;
        root.notice = "";
        root.fileName = name;
        root.special = "";
        root.stopSearch();
        if (root.picker)
            root.tabs = [{ cwd: root.cwd, back: [], forward: [] }];
        root.tab = 0;
        start = root.normalize(start);
        if (start === root.cwd)
            root.refresh(true);
        else
            root.cwd = start;
        root.generation++;
        root.open = true;
    }

    function hide(): void {
        if (root.picker)
            root.cancel();
        else
            root.close();
    }

    function close(): void {
        root.open = false;
        root.dialog = null;
        root.openWith = null;
        root.props = null;
        root.selected = [];
    }

    signal refreshing(bool clear)

    onCwdChanged: root.refresh(true)

    function refresh(clear: var): void {
        root.refreshing(clear === true);
        if (clear === true)
            root.entries = [];
        root.error = "";
        if (lister.running)
            root.pending = true;
        else
            root.startLister();
    }

    function startLister(): void {
        lister.command = ["find", root.cwd, "-mindepth", "1", "-maxdepth", "1", "-printf", "%y\\0%Y\\0%s\\0%T@\\0%f\\0"];
        lister.running = true;
    }

    function navigate(path: string): void {
        path = root.normalize(path);
        if (path === root.cwd && root.special.length === 0)
            return;
        root.backStack = root.arr(root.backStack).concat([root.cwd]);
        root.forwardStack = [];
        root.moveTo(path);
    }

    function back(): void {
        if (root.backStack.length === 0)
            return;
        const target = root.backStack[root.backStack.length - 1];
        root.forwardStack = root.arr(root.forwardStack).concat([root.cwd]);
        root.backStack = root.backStack.slice(0, -1);
        root.moveTo(target);
    }

    function forward(): void {
        if (root.forwardStack.length === 0)
            return;
        const target = root.forwardStack[root.forwardStack.length - 1];
        root.backStack = root.arr(root.backStack).concat([root.cwd]);
        root.forwardStack = root.forwardStack.slice(0, -1);
        root.moveTo(target);
    }

    function up(): void {
        root.navigate(root.parentOf(root.cwd));
    }

    function moveTo(path: string): void {
        root.selected = [];
        root.query = "";
        root.stopSearch();
        root.special = "";
        root.cwd = path;
        root.syncTab();
    }

    function syncTab(): void {
        const list = root.arr(root.tabs);
        list[root.tab] = { cwd: root.cwd, back: root.arr(root.backStack), forward: root.arr(root.forwardStack) };
        root.tabs = list;
    }

    onBackStackChanged: root.syncTab()
    onForwardStackChanged: root.syncTab()

    function newTab(path: string): void {
        const target = path.length > 0 ? root.normalize(path) : root.cwd;
        root.tabs = root.arr(root.tabs).concat([{ cwd: target, back: [], forward: [] }]);
        root.switchTab(root.tabs.length - 1);
    }

    function closeTab(index: int): void {
        if (root.tabs.length <= 1) {
            root.close();
            return;
        }
        const list = root.arr(root.tabs);
        list.splice(index, 1);
        root.tabs = list;
        root.switchTab(Math.min(root.tab > index ? root.tab - 1 : root.tab, list.length - 1), true);
    }

    function switchTab(index: int, force: bool): void {
        if (index < 0 || index >= root.tabs.length || (index === root.tab && !force))
            return;
        const t = root.tabs[index];
        root.tab = index;
        root.selected = [];
        root.query = "";
        root.stopSearch();
        root.special = "";
        root.backStack = t.back;
        root.forwardStack = t.forward;
        if (t.cwd === root.cwd)
            root.refresh(true);
        else
            root.cwd = t.cwd;
    }

    function cycleTab(delta: int): void {
        const n = root.tabs.length;
        root.switchTab(((root.tab + delta) % n + n) % n, false);
    }

    function showRecent(): void {
        if (root.special === "recent")
            return;
        root.selected = [];
        root.query = "";
        root.stopSearch();
        root.special = "recent";
        root.loadRecent();
    }

    function loadRecent(): void {
        recentScan.running = false;
        recentStat.running = false;
        recentScan.running = true;
    }

    function acceptRecentScan(text: string): void {
        const times = {};
        for (const line of text.split("\n")) {
            const tab = line.indexOf("\t");
            if (tab < 0)
                continue;
            const t = Date.parse(line.slice(0, tab));
            let path = line.slice(tab + 1);
            try {
                path = decodeURIComponent(path);
            } catch (e) {
                continue;
            }
            if (!isNaN(t) && (times[path] ?? 0) < t)
                times[path] = t;
        }
        for (const r of root.ownRecent)
            if ((times[r.path] ?? 0) < r.t)
                times[r.path] = r.t;
        const paths = Object.keys(times).sort((a, b) => times[b] - times[a]).slice(0, 150);
        if (paths.length === 0) {
            root.recentEntries = [];
            return;
        }
        recentStat.times = times;
        recentStat.command = ["sh", "-c", 'for p in "$@"; do stat -L --printf "%F\\0%s\\0%Y\\0%n\\0" -- "$p" 2>/dev/null; done', "sh"].concat(paths);
        recentStat.running = true;
    }

    function acceptRecentStat(text: string): void {
        const parts = text.split("\0");
        const out = [];
        for (let i = 0; i + 3 < parts.length; i += 4) {
            const path = parts[i + 3];
            const name = root.basename(path);
            const dot = name.lastIndexOf(".");
            const mtime = Number(parts[i + 2]);
            out.push({
                name: name,
                path: path,
                isDir: parts[i] === "directory",
                suffix: dot > 0 ? name.slice(dot + 1) : "",
                size: Number(parts[i + 1]),
                mtime: mtime,
                modified: new Date(mtime * 1000),
                hidden: name.startsWith("."),
                location: root.pretty(root.parentOf(path)),
                used: recentStat.times[path] ?? 0
            });
        }
        out.sort((a, b) => b.used - a.used);
        root.recentEntries = out;
    }

    function recordRecent(path: string): void {
        const list = root.ownRecent.filter(r => r.path !== path);
        list.unshift({ path: path, t: Date.now() });
        root.ownRecent = list.slice(0, 200);
        pinsSave.restart();
    }

    function askOpenWith(): void {
        const e = root.selectedEntries[0];
        if (root.selected.length !== 1 || !e || e.isDir)
            return;
        root.openWith = { path: e.path, name: e.name, mime: "", apps: [], ready: false };
        mimeProc.command = ["sh", "-c",
            'mime=$(xdg-mime query filetype "$1"); printf "%s\\n" "$mime"; '
            + 'pat=$(printf "%s" "$mime" | sed "s/[+.]/\\\\&/g"); case "$mime" in text/*) pat="($pat|text/plain)";; esac; '
            + '{ xdg-mime query default "$mime" 2>/dev/null; for d in $(printf "%s" "${XDG_DATA_DIRS:-/usr/share}" | tr ":" " ") "${XDG_DATA_HOME:-$HOME/.local/share}"; do '
            + '[ -d "$d/applications" ] && grep -lsE "^MimeType=.*(^|;)$pat(;|$)" "$d/applications/"*.desktop; done 2>/dev/null | xargs -rn1 basename; } | awk "!seen[\\$0]++" | while IFS= read -r id; do '
            + 'for d in "${XDG_DATA_HOME:-$HOME/.local/share}" $(printf "%s" "${XDG_DATA_DIRS:-/usr/share}" | tr ":" " "); do f="$d/applications/$id"; [ -f "$f" ] || continue; '
            + 'n=$(sed -n "s/^Name=//p" "$f" | head -n1); i=$(sed -n "s/^Icon=//p" "$f" | head -n1); printf "%s\\t%s\\t%s\\n" "$id" "$n" "$i"; break; done; done',
            "sh", e.path];
        mimeProc.running = true;
    }

    function launchWith(entry: var, always: bool): void {
        const info = root.openWith;
        root.openWith = null;
        if (!info)
            return;
        const id = entry.id.endsWith(".desktop") ? entry.id : entry.id + ".desktop";
        if (always && info.mime.length > 0)
            Quickshell.execDetached(["xdg-mime", "default", id, info.mime]);
        if (entry.command) {
            const cmd = root.arr(entry.command).filter(a => !/^%[fFuUdDnNickvm]$/.test(a));
            Quickshell.execDetached({ command: cmd.concat([info.path]), workingDirectory: entry.workingDirectory });
        } else {
            Quickshell.execDetached(["sh", "-c", "id=\"$1\"; file=\"$2\"; for d in \"${XDG_DATA_HOME:-$HOME/.local/share}\" $(printf \"%s\" \"${XDG_DATA_DIRS:-/usr/share}\" | tr \":\" \" \"); do f=\"$d/applications/$id\"; [ -f \"$f\" ] || continue; exec=$(sed -n \"s/^Exec=//p\" \"$f\" | head -n1); q=$(printf \"%s\" \"$file\" | sed \"s/'/'\\\\\\\\''/g\"); case \"$exec\" in *%[fFuU]*) cmd=\"${exec%%\\%[fFuU]*}'$q'${exec#*\\%[fFuU]}\";; *) cmd=\"$exec '$q'\";; esac; cmd=$(printf \"%s\" \"$cmd\" | sed \"s/ %[dDnNickvm]//g\"); exec sh -c \"$cmd\"; done", "sh", id, info.path]);
        }
        root.recordRecent(info.path);
        root.close();
    }

    function askProperties(): void {
        const e = root.selectedEntries[0];
        if (!e && root.selected.length > 0)
            return;
        const path = e ? e.path : root.cwd;
        root.props = { path: path, name: e ? e.name : (root.basename(root.cwd) || "/"), isDir: e ? e.isDir : true, rows: [], size: "" };
        propsProc.command = ["sh", "-c",
            'p="$1"; stat --printf "%F\\n%A (%a)\\n%U:%G\\n%s\\n%Y\\n%X\\n%N\\n" -- "$p"; xdg-mime query filetype "$p" 2>/dev/null || echo; if [ -d "$p" ]; then find "$p" -mindepth 1 -maxdepth 1 2>/dev/null | wc -l; fi',
            "sh", path];
        propsProc.running = true;
        if (e && e.isDir) {
            propsSizer.running = false;
            propsSizer.command = ["du", "-sb", "--", path];
            propsSizer.running = true;
        }
    }

    function acceptProps(text: string): void {
        const p = root.props;
        if (!p)
            return;
        const l = text.split("\n");
        const rows = [];
        const link = (l[6] ?? "").match(/-> '(.*)'$/);
        rows.push(["Name", p.name]);
        rows.push(["Type", (l[7] ?? "").length > 0 ? l[7] : (l[0] ?? "")]);
        rows.push(["Size", p.isDir ? ((l[8] ?? "").trim().length > 0 ? `${l[8].trim()} items` : "") : root.formatSize(Number(l[3] ?? 0)) + ` (${String(l[3] ?? 0).replace(/\B(?=(\d{3})+(?!\d))/g, ",")} bytes)`]);
        rows.push(["Location", root.pretty(root.parentOf(p.path))]);
        if (link)
            rows.push(["Link target", link[1]]);
        rows.push(["Modified", root.formatFull(l[4] ?? "")]);
        rows.push(["Accessed", root.formatFull(l[5] ?? "")]);
        rows.push(["Permissions", l[1] ?? ""]);
        rows.push(["Owner", l[2] ?? ""]);
        root.props = Object.assign({}, p, { rows: rows });
    }

    function formatFull(epoch: string): string {
        const n = Number(epoch);
        return isNaN(n) ? epoch : Qt.formatDateTime(new Date(n * 1000), "d MMM yyyy, HH:mm");
    }

    function startSearch(): void {
        root.query = "";
        root.selected = [];
        root.searching = true;
    }

    function stopSearch(): void {
        if (!root.searching)
            return;
        root.searching = false;
        root.searchQuery = "";
        root.searchResults = [];
        root.selected = [];
        finder.running = false;
        searchDebounce.stop();
    }

    function setSearchQuery(text: string): void {
        root.searchQuery = text;
        const q = text.trim();
        if (q.length < 2) {
            root.searchResults = [];
            finder.running = false;
            searchDebounce.stop();
            return;
        }
        searchDebounce.restart();
    }

    function runSearch(): void {
        finder.running = false;
        const q = root.searchQuery.trim();
        if (q.length < 2)
            return;
        finder.query = q;
        const words = q.split(/\s+/).filter(w => w.length > 0);
        const longest = words.reduce((a, b) => b.length > a.length ? b : a, "");
        finder.command = ["sh", "-c",
            'db="$1"; cwd="$2"; longest="$3"; shift 3; { plocate -d "$db" -i -l 300 -- "$@"; fd -i --hidden --fixed-strings --max-depth 6 --max-results 200 --absolute-path -- "$longest" "$cwd"; } 2>/dev/null '
            + '| sort -u | head -n 500 | while IFS= read -r p; do if [ -d "$p" ]; then printf "d\\0%s\\0" "$p"; else printf "f\\0%s\\0" "$p"; fi; done',
            "sh", SpotlightConfig.indexDb, root.cwd, longest, ...words];
        finder.running = true;
    }

    function acceptSearch(text: string): void {
        const q = finder.query;
        if (!root.searching || q !== root.searchQuery.trim())
            return;
        const parts = text.split("\0");
        const seen = {};
        const out = [];
        for (let i = 0; i + 1 < parts.length; i += 2) {
            const path = parts[i + 1].replace(/\/+$/, "");
            if (path.length === 0 || seen[path] || path === root.cwd)
                continue;
            seen[path] = true;
            const name = root.basename(path);
            const dir = root.parentOf(path);
            const score = root.scoreWords(q, name, path);
            if (score < 0)
                continue;
            const dot = name.lastIndexOf(".");
            out.push({
                name: name,
                path: path,
                isDir: parts[i] === "d",
                suffix: dot > 0 ? name.slice(dot + 1) : "",
                size: 0,
                mtime: 0,
                modified: null,
                hidden: name.startsWith("."),
                location: root.pretty(dir),
                score: score + (dir === root.cwd ? 0.3 : path.startsWith(root.cwd + "/") ? 0.15 : 0)
            });
        }
        out.sort((a, b) => b.score - a.score);
        root.searchResults = out.slice(0, 150);
    }

    function scoreWords(q: string, name: string, path: string): real {
        let total = 0;
        const words = q.split(/\s+/).filter(w => w.length > 0);
        for (const w of words) {
            const s = Math.max(Fuzzy.score(w, name), Fuzzy.score(w, path) * 0.8);
            if (s < 0)
                return -1;
            total += s;
        }
        return words.length > 0 ? total / words.length : -1;
    }

    function reveal(entry: var): void {
        root.navigate(root.parentOf(entry.path));
        root.selected = [entry.path];
    }

    function pretty(path: string): string {
        return path === root.home ? "~" : path.startsWith(root.home + "/") ? "~" + path.slice(root.home.length) : path;
    }

    function isPinned(path: string): bool {
        return root.pinned.some(p => p.path === path);
    }

    function setView(mode: string): void {
        root.view = mode;
        pinsSave.restart();
    }

    function pin(path: string): void {
        if (root.isPinned(path))
            return;
        root.pinned = root.pinned.concat([{ name: root.basename(path) || "/", path: path }]);
        pinsSave.restart();
        root.flash("Pinned to sidebar");
    }

    function unpin(path: string): void {
        root.pinned = root.pinned.filter(p => p.path !== path);
        pinsSave.restart();
    }

    property bool pinsReady: false

    function loadPins(text: string): void {
        if (root.pinsReady)
            return;
        root.pinsReady = true;
        let stored = [];
        try {
            const data = JSON.parse(text);
            const list = Array.isArray(data) ? data : (data.pinned ?? []);
            stored = list.filter(p => typeof p.path === "string" && typeof p.name === "string");
            if (data.view === "grid" || data.view === "list")
                root.view = data.view;
            if (Array.isArray(data.recent))
                root.ownRecent = data.recent.filter(r => typeof r.path === "string" && typeof r.t === "number").concat(root.ownRecent).slice(0, 200);
        } catch (e) {
        }
        const early = root.pinned.filter(p => !stored.some(s => s.path === p.path));
        root.pinned = stored.concat(early);
        if (early.length > 0)
            pinsSave.restart();
    }

    function dropOn(dest: string, paths: var, copy: bool): void {
        const list = root.arr(paths).filter(p => p !== dest && root.parentOf(p) !== dest && !dest.startsWith(p + "/"));
        if (list.length === 0 || root.busy)
            return;
        const args = [];
        for (const src of list)
            args.push(src, root.join(dest, root.basename(src)));
        const cmd = copy ? "cp -a" : "mv";
        root.exec(["sh", "-c", `while [ $# -ge 2 ]; do [ -e "$2" ] && { echo "$2 already exists" >&2; exit 1; }; ${cmd} -- "$1" "$2" || exit 1; shift 2; done`, "sh", ...args],
            `${list.length === 1 ? root.basename(list[0]) : list.length + " items"} ${copy ? "copied" : "moved"} to ${root.basename(dest) || "/"}`);
    }

    function setSort(field: string): void {
        if (root.sortBy === field)
            root.sortDesc = !root.sortDesc;
        else {
            root.sortBy = field;
            root.sortDesc = field !== "name";
        }
    }

    function activate(entry: var): void {
        if (entry.isDir)
            root.navigate(entry.path);
        else if (root.save)
            root.fileName = entry.name;
        else if (!root.picker) {
            root.openExternal(entry.path);
            root.close();
        }
        else if (!root.directory)
            root.finish([entry.path]);
    }

    function select(entry: var, toggle: bool, range: bool): void {
        if (root.directory && !entry.isDir) {
            root.selected = [];
            return;
        }
        if (root.save && !entry.isDir) {
            root.fileName = entry.name;
            root.selected = [entry.path];
            return;
        }
        if (!root.multiple || (!toggle && !range)) {
            root.selected = [entry.path];
            return;
        }
        if (range && root.selected.length > 0) {
            const list = root.items;
            const anchor = list.findIndex(e => e.path === root.selected[0]);
            const target = list.findIndex(e => e.path === entry.path);
            if (anchor >= 0 && target >= 0) {
                const lo = Math.min(anchor, target);
                const hi = Math.max(anchor, target);
                root.selected = list.slice(lo, hi + 1).filter(e => root.directory ? e.isDir : (!root.picker || !e.isDir)).map(e => e.path);
                return;
            }
        }
        if (root.selected.includes(entry.path))
            root.selected = root.selected.filter(p => p !== entry.path);
        else
            root.selected = root.arr(root.selected).concat([entry.path]);
    }

    function selectAll(): void {
        if (!root.multiple)
            return;
        root.selected = root.items.filter(e => root.directory ? e.isDir : (!root.picker || !e.isDir)).map(e => e.path);
    }

    onSelectedChanged: {
        sizer.running = false;
        root.folderSize = "";
        root.sizedPath = "";
        if (root.selected.length === 1 && root.isDirPath(root.selected[0])) {
            root.sizedPath = root.selected[0];
            sizer.command = ["du", "-sb", "--", root.sizedPath];
            sizer.running = true;
        }
    }

    function accept(): void {
        if (!root.canAccept)
            return;
        if (root.save)
            root.finish([root.join(root.cwd, root.fileName.trim())]);
        else if (root.directory)
            root.finish(root.selected.length > 0 ? root.selected : [root.cwd]);
        else if (root.selected.length === 1 && root.isDirPath(root.selected[0]))
            root.navigate(root.selected[0]);
        else
            root.finish(root.selected);
    }

    function cancel(): void {
        root.finish([]);
    }

    function pickSave(dir: string, name: string, callback: var): void {
        if (root.open)
            root.finish([]);
        root.picker = true;
        root.multiple = false;
        root.directory = false;
        root.save = true;
        root.outFile = "";
        root.saveCallback = callback;
        root.begin(dir.length > 0 && dir.startsWith("/") ? dir : root.home, name);
    }

    function finish(paths: var): void {
        const out = root.outFile;
        const cb = root.saveCallback;
        const list = [];
        for (let i = 0; i < paths.length; i++)
            list.push(paths[i]);
        root.saveCallback = null;
        root.close();
        root.outFile = "";
        if (cb) {
            if (list.length > 0)
                cb(list[0]);
            return;
        }
        if (out.length === 0)
            return;
        if (list.length === 0)
            Quickshell.execDetached(["touch", out + ".done"]);
        else
            Quickshell.execDetached(["sh", "-c", 'o="$1"; shift; printf "%s\\n" "$@" > "$o"; touch "$o.done"', "sh", out].concat(list));
    }

    function openExternal(path: string): void {
        const ext = path.slice(path.lastIndexOf(".") + 1).toLowerCase();
        if (["png", "jpg", "jpeg", "gif", "webp", "bmp"].includes(ext))
            Preview.openFile(path);
        else
            Quickshell.execDetached(["xdg-open", path]);
        root.recordRecent(path);
    }

    function uriList(paths: var): string {
        return root.arr(paths).map(p => "file://" + p.split("/").map(encodeURIComponent).join("/")).join("\r\n");
    }

    function openTerminal(path: var): void {
        const dir = typeof path === "string" && path.length > 0 ? path : root.cwd;
        const term = SpotlightConfig.terminal;
        const cmd = term === "wezterm" ? ["wezterm", "start", "--cwd", dir]
            : term === "kitty" ? ["kitty", "--directory", dir]
            : term === "alacritty" ? ["alacritty", "--working-directory", dir]
            : [term];
        Quickshell.execDetached({ command: cmd, workingDirectory: dir });
        root.close();
    }

    function copyPath(): void {
        const paths = root.selected.length > 0 ? root.selected : [root.cwd];
        Quickshell.execDetached(["sh", "-c", 'printf "%s\\n" "$@" | wl-copy -n', "sh"].concat(root.arr(paths)));
        root.flash(paths.length === 1 ? "Path copied" : `${paths.length} paths copied`);
    }

    function cut(): void {
        if (root.selected.length === 0)
            return;
        root.clip = { op: "cut", paths: root.arr(root.selected) };
        root.flash(`${root.selected.length} ${root.selected.length === 1 ? "item" : "items"} cut`);
    }

    function copy(): void {
        if (root.selected.length === 0)
            return;
        root.clip = { op: "copy", paths: root.arr(root.selected) };
        root.flash(`${root.selected.length} ${root.selected.length === 1 ? "item" : "items"} copied`);
    }

    function paste(): void {
        if (root.clip.paths.length === 0 || root.busy)
            return;
        const args = [];
        const taken = root.entries.map(e => e.name);
        for (const src of root.clip.paths) {
            const name = root.basename(src);
            if (root.clip.op === "cut" && root.parentOf(src) === root.cwd)
                continue;
            if (root.cwd.startsWith(src + "/") || root.cwd === src) {
                root.flash("Cannot paste a folder into itself");
                return;
            }
            const dest = root.uniqueName(name, taken);
            taken.push(dest);
            args.push(src, root.join(root.cwd, dest));
        }
        if (args.length === 0)
            return;
        const cmd = root.clip.op === "cut" ? "mv" : "cp -a";
        root.exec(["sh", "-c", `while [ $# -ge 2 ]; do ${cmd} -- "$1" "$2" || exit 1; shift 2; done`, "sh", ...args], root.clip.op === "cut" ? "Moved" : "Pasted");
        if (root.clip.op === "cut")
            root.clip = { op: "", paths: [] };
    }

    function askNewFolder(): void {
        root.dialog = { kind: "newFolder", title: "New folder", value: root.uniqueName("New folder", root.entries.map(e => e.name)), action: "Create", field: true };
    }

    function askRename(): void {
        if (root.selected.length !== 1)
            return;
        const e = root.selectedEntries[0];
        if (!e)
            return;
        root.dialog = { kind: "rename", title: `Rename "${e.name}"`, value: e.name, action: "Rename", field: true, path: e.path };
    }

    function askDelete(permanent: bool): void {
        if (root.selected.length === 0)
            return;
        const n = root.selected.length;
        const what = n === 1 ? `"${root.basename(root.selected[0])}"` : `${n} items`;
        root.dialog = permanent
            ? { kind: "delete", title: `Permanently delete ${what}?`, body: "This cannot be undone.", action: "Delete", danger: true, paths: root.arr(root.selected) }
            : { kind: "trash", title: `Move ${what} to trash?`, body: "You can restore it from the trash later.", action: "Trash", danger: true, paths: root.arr(root.selected) };
    }

    function confirmDialog(value: string): void {
        const d = root.dialog;
        root.dialog = null;
        if (!d)
            return;
        if (d.kind === "newFolder") {
            if (!root.validName(value))
                return root.flash("Invalid name");
            root.exec(["mkdir", "--", root.join(root.cwd, value.trim())], "Folder created", value.trim());
        } else if (d.kind === "rename") {
            const name = value.trim();
            if (!root.validName(name))
                return root.flash("Invalid name");
            if (name === root.basename(d.path))
                return;
            if (root.entries.some(e => e.name === name))
                return root.flash("An item with that name already exists");
            root.exec(["mv", "--", d.path, root.join(root.cwd, name)], "Renamed", name);
        } else if (d.kind === "delete") {
            root.exec(["rm", "-rf", "--", ...d.paths], "Deleted");
        } else if (d.kind === "trash") {
            const script = 't="${XDG_DATA_HOME:-$HOME/.local/share}/Trash"; mkdir -p "$t/files" "$t/info"; '
                + 'for f in "$@"; do n=$(basename -- "$f"); d="$n"; i=1; '
                + 'while [ -e "$t/files/$d" ] || [ -e "$t/info/$d.trashinfo" ]; do d="$n.$i"; i=$((i+1)); done; '
                + 'printf "[Trash Info]\\nPath=%s\\nDeletionDate=%s\\n" "$f" "$(date +%Y-%m-%dT%H:%M:%S)" > "$t/info/$d.trashinfo"; '
                + 'mv -- "$f" "$t/files/$d" || exit 1; done';
            root.exec(["sh", "-c", script, "sh", ...d.paths], "Moved to trash");
        }
    }

    function exec(cmd: var, done: string, selectName: var): void {
        if (root.busy)
            return root.flash("Another operation is still running");
        op.doneMessage = done;
        op.selectName = selectName ?? "";
        op.command = cmd;
        op.running = true;
    }

    function flash(text: string): void {
        root.notice = text;
        noticeTimer.restart();
    }

    function arr(list: var): var {
        const out = [];
        for (let i = 0; i < list.length; i++)
            out.push(list[i]);
        return out;
    }

    function validName(name: string): bool {
        const n = name.trim();
        return n.length > 0 && !n.includes("/") && n !== "." && n !== "..";
    }

    function uniqueName(name: string, taken: var): string {
        if (!taken.includes(name))
            return name;
        const dot = name.lastIndexOf(".");
        const stem = dot > 0 ? name.slice(0, dot) : name;
        const ext = dot > 0 ? name.slice(dot) : "";
        let i = 2;
        let candidate = `${stem} (copy)${ext}`;
        while (taken.includes(candidate))
            candidate = `${stem} (copy ${i++})${ext}`;
        return candidate;
    }

    function isDirPath(path: string): bool {
        return root.visibleEntries.some(e => e.path === path && e.isDir);
    }

    function basename(path: string): string {
        const i = path.lastIndexOf("/");
        return i < 0 ? path : path.slice(i + 1);
    }

    function parentOf(path: string): string {
        if (path === "/")
            return "/";
        const i = path.lastIndexOf("/");
        return i <= 0 ? "/" : path.slice(0, i);
    }

    function join(dir: string, name: string): string {
        return dir === "/" ? "/" + name : dir + "/" + name;
    }

    function normalize(path: string): string {
        if (path.startsWith("~"))
            path = root.home + path.slice(1);
        const parts = [];
        for (const part of path.split("/")) {
            if (part === "" || part === ".")
                continue;
            if (part === "..")
                parts.pop();
            else
                parts.push(part);
        }
        return "/" + parts.join("/");
    }

    readonly property var crumbs: {
        if (root.special === "recent")
            return [{ name: "Recent", path: "" }];
        const out = [{ name: "/", path: "/" }];
        let acc = "";
        for (const part of root.cwd.split("/").filter(p => p.length > 0)) {
            acc += "/" + part;
            out.push({ name: part, path: acc });
        }
        return out;
    }

    function iconFor(entry: var): string {
        if (entry.isDir)
            return "folder";
        const ext = entry.suffix.toLowerCase();
        if (["png", "jpg", "jpeg", "gif", "webp", "svg", "bmp", "ico", "avif", "heic"].includes(ext))
            return "image";
        if (["mp4", "mkv", "webm", "mov", "avi", "m4v"].includes(ext))
            return "movie";
        if (["mp3", "flac", "wav", "ogg", "m4a", "opus", "aac"].includes(ext))
            return "music_note";
        if (["zip", "tar", "gz", "xz", "zst", "bz2", "7z", "rar"].includes(ext))
            return "archive";
        if (ext === "pdf")
            return "picture_as_pdf";
        if (["txt", "md", "org", "rst", "log"].includes(ext))
            return "subject";
        if (["js", "ts", "py", "rs", "c", "cpp", "h", "hpp", "go", "java", "nix", "qml", "sh", "lua", "html", "css", "json", "toml", "yaml", "yml", "xml"].includes(ext))
            return "code";
        return "insert_drive_file";
    }

    function formatSize(bytes: real): string {
        if (bytes < 1024)
            return `${bytes} B`;
        const units = ["KB", "MB", "GB", "TB"];
        let v = bytes / 1024;
        let i = 0;
        while (v >= 1024 && i < units.length - 1) {
            v /= 1024;
            i++;
        }
        return `${v < 10 ? v.toFixed(1) : Math.round(v)} ${units[i]}`;
    }

    function formatDate(d: var): string {
        if (!d)
            return "";
        const now = new Date();
        if (d.toDateString() === now.toDateString())
            return Qt.formatDateTime(d, "'Today' HH:mm");
        return Qt.formatDateTime(d, d.getFullYear() === now.getFullYear() ? "d MMM HH:mm" : "d MMM yyyy");
    }

    function parse(text: string): void {
        const parts = text.split("\0");
        const list = [];
        for (let i = 0; i + 4 < parts.length; i += 5) {
            const name = parts[i + 4];
            const dot = name.lastIndexOf(".");
            const mtime = Number(parts[i + 3]);
            list.push({
                name: name,
                path: root.join(root.cwd, name),
                isDir: parts[i] === "d" || (parts[i] === "l" && parts[i + 1] === "d"),
                suffix: dot > 0 ? name.slice(dot + 1) : "",
                size: Number(parts[i + 2]),
                mtime: mtime,
                modified: new Date(mtime * 1000),
                hidden: name.startsWith(".")
            });
        }
        root.entries = list;
    }

    function naturalCompare(a: string, b: string): int {
        const ax = a.toLowerCase().match(/(\d+|\D+)/g) ?? [];
        const bx = b.toLowerCase().match(/(\d+|\D+)/g) ?? [];
        for (let i = 0; i < Math.min(ax.length, bx.length); i++) {
            const an = /^\d/.test(ax[i]);
            const bn = /^\d/.test(bx[i]);
            if (an && bn) {
                const d = Number(ax[i]) - Number(bx[i]);
                if (d !== 0)
                    return d;
            } else if (ax[i] !== bx[i]) {
                return ax[i] < bx[i] ? -1 : 1;
            }
        }
        return ax.length - bx.length;
    }

    function loadPlaces(text: string): void {
        const dirs = {};
        for (const line of text.split("\n")) {
            const m = line.match(/^XDG_(\w+)_DIR="(.*)"$/);
            if (m)
                dirs[m[1]] = m[2].replace("$HOME", root.home).replace(/\/+$/, "");
        }
        const list = [{ name: "Home", icon: "home", path: root.home }];
        const known = [["DESKTOP", "Desktop", "desktop_windows"], ["DOCUMENTS", "Documents", "description"], ["DOWNLOAD", "Downloads", "file_download"], ["PICTURES", "Pictures", "image"], ["MUSIC", "Music", "music_note"], ["VIDEOS", "Videos", "movie"]];
        for (const [key, name, icon] of known) {
            const p = dirs[key] ?? root.join(root.home, name);
            if (p !== root.home)
                list.push({ name: name, icon: icon, path: p });
        }
        list.push({ name: "Trash", icon: "delete", path: (Quickshell.env("XDG_DATA_HOME") ?? root.home + "/.local/share") + "/Trash/files" });
        list.push({ name: "System", icon: "storage", path: "/" });
        root.places = list;
    }

    Process {
        id: lister

        stdout: StdioCollector {
            onStreamFinished: {
                if (!root.pending)
                    root.parse(text);
            }
        }

        onExited: code => {
            if (root.pending) {
                root.pending = false;
                root.startLister();
            } else if (code !== 0 && root.entries.length === 0) {
                root.error = "Cannot open this folder";
            }
        }
    }

    Process {
        id: sizer

        stdout: StdioCollector {
            onStreamFinished: {
                const n = Number(text.split("\t")[0]);
                if (!isNaN(n) && sizer.command[3] === root.sizedPath)
                    root.folderSize = root.formatSize(n);
            }
        }
    }

    Process {
        id: op

        property string doneMessage: ""
        property string selectName: ""
        property string errorText: ""

        stderr: StdioCollector {
            onStreamFinished: op.errorText = text.trim()
        }

        onExited: code => {
            if (code === 0) {
                root.flash(op.doneMessage);
                if (op.selectName.length > 0)
                    root.selected = [root.join(root.cwd, op.selectName)];
            } else {
                const line = op.errorText.split("\n")[0].replace(/^[^:]*: /, "");
                root.flash(line.length > 0 ? line : "Operation failed");
            }
            root.refresh();
        }
    }

    Process {
        id: finder

        property string query: ""

        stdout: StdioCollector {
            onStreamFinished: root.acceptSearch(text)
        }
    }

    Process {
        id: recentScan

        command: ["sh", "-c", "f=\"${XDG_DATA_HOME:-$HOME/.local/share}/recently-used.xbel\"; [ -f \"$f\" ] || exit 0; sed -n 's/^ *<bookmark href=\"file:\\/\\/\\([^\"]*\\)\" added=\"[^\"]*\" modified=\"\\([^\"]*\\)\".*/\\2\\t\\1/p' \"$f\" | sort -r | head -n 400"]

        stdout: StdioCollector {
            onStreamFinished: root.acceptRecentScan(text)
        }
    }

    Process {
        id: recentStat

        property var times: ({})

        stdout: StdioCollector {
            onStreamFinished: root.acceptRecentStat(text)
        }
    }

    Process {
        id: mimeProc

        stdout: StdioCollector {
            onStreamFinished: {
                if (!root.openWith)
                    return;
                const lines = text.split("\n").filter(l => l.length > 0);
                const apps = lines.slice(1).map(l => l.split("\t")).filter(f => f.length >= 2).map(f => ({ id: f[0], name: f[1], icon: f[2] ?? "" }));
                root.openWith = Object.assign({}, root.openWith, { mime: lines[0] ?? "", apps: apps, ready: true });
            }
        }
    }

    Process {
        id: propsProc

        stdout: StdioCollector {
            onStreamFinished: root.acceptProps(text)
        }
    }

    Process {
        id: propsSizer

        stdout: StdioCollector {
            onStreamFinished: {
                const n = Number(text.split("\t")[0]);
                if (root.props && !isNaN(n))
                    root.props = Object.assign({}, root.props, { size: root.formatSize(n) });
            }
        }
    }

    Timer {
        id: searchDebounce
        interval: SpotlightConfig.fileDebounce
        onTriggered: root.runSearch()
    }

    FileView {
        id: pinsFile
        path: SpotlightConfig.stateDir + "/files.json"
        printErrors: false
        onLoaded: root.loadPins(text())
        onLoadFailed: root.pinsReady = true
    }

    Timer {
        id: pinsSave
        interval: 300
        onTriggered: pinsFile.setText(JSON.stringify({ pinned: root.pinned, view: root.view, recent: root.ownRecent }))
    }

    Timer {
        id: noticeTimer
        interval: 3500
        onTriggered: root.notice = ""
    }

    FileView {
        path: root.home + "/.config/user-dirs.dirs"
        onLoaded: root.loadPlaces(text())
        onLoadFailed: root.loadPlaces("")
    }
}
