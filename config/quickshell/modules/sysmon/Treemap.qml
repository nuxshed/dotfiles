pragma ComponentBehavior: Bound

import QtQuick
import "../../config"
import "../../services"

Item {
    id: root

    property var entries: []
    property var rects: []
    property int hovered: -1
    readonly property var palette: [Colors.blue, Colors.cyan, Colors.green, Colors.yellow, Colors.orange, Colors.magenta, Colors.red]

    signal activated(var entry)
    signal contextRequested(var entry, real x, real y)

    onEntriesChanged: layout()
    onWidthChanged: layout()
    onHeightChanged: layout()

    function layout(): void {
        const total = root.entries.reduce((a, e) => a + e.size, 0);
        if (width <= 0 || height <= 0 || total <= 0) {
            root.rects = [];
            return;
        }
        const items = root.entries.map((e, i) => ({ i, area: e.size / total * width * height })).filter(e => e.area >= 1);
        const out = [];
        let x = 0, y = 0, w = width, h = height;
        let row = [];
        const worst = (r, s) => {
            const sum = r.reduce((a, e) => a + e.area, 0);
            let mx = 0, mn = Infinity;
            for (const e of r) { mx = Math.max(mx, e.area); mn = Math.min(mn, e.area); }
            return Math.max(s * s * mx / (sum * sum), sum * sum / (s * s * mn));
        };
        const place = () => {
            const sum = row.reduce((a, e) => a + e.area, 0);
            const horiz = w >= h;
            const s = horiz ? h : w;
            const thick = sum / s;
            let off = 0;
            for (const e of row) {
                const len = e.area / thick;
                out.push(horiz ? { i: e.i, x, y: y + off, w: thick, h: len } : { i: e.i, x: x + off, y, w: len, h: thick });
                off += len;
            }
            if (horiz) { x += thick; w -= thick; } else { y += thick; h -= thick; }
            row = [];
        };
        for (const it of items) {
            const s = Math.min(w, h);
            if (row.length > 0 && worst(row.concat([it]), s) > worst(row, s))
                place();
            row.push(it);
        }
        if (row.length > 0)
            place();
        root.rects = out;
    }

    Repeater {
        model: root.rects

        Rectangle {
            id: block
            required property var modelData
            readonly property var entry: root.entries[modelData.i]
            readonly property bool hot: root.hovered === modelData.i
            readonly property color tint: root.palette[modelData.i % root.palette.length]

            x: modelData.x + 1
            y: modelData.y + 1
            width: Math.max(0, modelData.w - 2)
            height: Math.max(0, modelData.h - 2)
            radius: 3
            color: Qt.alpha(tint, hot ? 0.55 : entry.dir ? 0.3 : entry.file ? 0.22 : 0.12)
            border.width: 1
            border.color: Qt.alpha(tint, hot ? 0.9 : 0.4)
            clip: true

            Behavior on color { ColorAnimation { duration: 100 } }

            Column {
                x: 6
                y: 4
                visible: block.width > 48 && block.height > 30
                width: parent.width - 12
                spacing: 1

                Text {
                    width: parent.width
                    text: block.entry.name
                    color: Colors.textBright
                    font.pixelSize: 11
                    font.family: Fonts.family
                    font.weight: Font.Medium
                    elide: Text.ElideRight
                }
                Text {
                    width: parent.width
                    text: SysMon.fmtBytes(block.entry.size, 1)
                    color: Colors.textDimmed
                    font.pixelSize: 10
                    font.family: Fonts.family
                    elide: Text.ElideRight
                }
            }

            MouseArea {
                anchors.fill: parent
                hoverEnabled: true
                acceptedButtons: Qt.LeftButton | Qt.RightButton
                cursorShape: block.entry.dir || block.entry.file === undefined ? Qt.PointingHandCursor : Qt.ArrowCursor
                onEntered: root.hovered = block.modelData.i
                onExited: if (root.hovered === block.modelData.i) root.hovered = -1
                onClicked: mouse => {
                    if (mouse.button === Qt.RightButton)
                        root.contextRequested(block.entry, block.x + mouse.x, block.y + mouse.y);
                    else
                        root.activated(block.entry);
                }
            }
        }
    }
}
