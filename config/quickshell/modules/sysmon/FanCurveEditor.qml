pragma ComponentBehavior: Bound

import QtQuick
import "../../config"

Item {
    id: root

    property var points: []
    property bool enabled: true
    property int dragging: -1
    property int hovered: -1

    signal changed()

    readonly property real padL: 34
    readonly property real padB: 22
    readonly property real padT: 10
    readonly property real padR: 12
    readonly property real plotW: width - padL - padR
    readonly property real plotH: height - padT - padB

    function px(t: real): real { return padL + (t - 30) / 70 * plotW; }
    function py(p: real): real { return padT + plotH - p / 255 * plotH; }
    function tOf(x: real): real { return Math.max(30, Math.min(100, (x - padL) / plotW * 70 + 30)); }
    function pOf(y: real): real { return Math.max(0, Math.min(255, (padT + plotH - y) / plotH * 255)); }

    function nearest(x: real, y: real): int {
        let best = -1, dist = 14;
        for (let i = 0; i < root.points.length; i++) {
            const d = Math.hypot(px(root.points[i].t) - x, py(root.points[i].pwm) - y);
            if (d < dist) { dist = d; best = i; }
        }
        return best;
    }

    function move(i: int, x: real, y: real): void {
        const pts = root.points.map(p => ({ t: p.t, pwm: p.pwm }));
        const t = Math.round(tOf(x)), pwm = Math.round(pOf(y));
        pts[i] = { t, pwm };
        for (let k = i + 1; k < pts.length; k++) {
            pts[k].t = Math.max(pts[k].t, t);
            pts[k].pwm = Math.max(pts[k].pwm, pwm);
        }
        for (let k = i - 1; k >= 0; k--) {
            pts[k].t = Math.min(pts[k].t, t);
            pts[k].pwm = Math.min(pts[k].pwm, pwm);
        }
        root.points = pts;
        root.changed();
    }

    onPointsChanged: canvas.requestPaint()
    onEnabledChanged: canvas.requestPaint()
    onHoveredChanged: canvas.requestPaint()
    onDraggingChanged: canvas.requestPaint()

    Canvas {
        id: canvas
        anchors.fill: parent
        onWidthChanged: requestPaint()
        onHeightChanged: requestPaint()

        onPaint: {
            const ctx = getContext("2d");
            ctx.clearRect(0, 0, width, height);
            ctx.font = "9px 'Google Sans Flex'";
            ctx.fillStyle = Colors.textMuted;
            ctx.strokeStyle = Qt.alpha(Colors.textMuted, 0.15);
            ctx.lineWidth = 1;
            ctx.textAlign = "right";
            for (const pct of [0, 25, 50, 75, 100]) {
                const y = Math.round(root.py(pct / 100 * 255)) + 0.5;
                ctx.beginPath();
                ctx.moveTo(root.padL, y);
                ctx.lineTo(width - root.padR, y);
                ctx.stroke();
                ctx.fillText(`${pct}%`, root.padL - 6, y + 3);
            }
            ctx.textAlign = "center";
            for (let t = 30; t <= 100; t += 10) {
                const x = Math.round(root.px(t)) + 0.5;
                ctx.beginPath();
                ctx.moveTo(x, root.padT);
                ctx.lineTo(x, root.padT + root.plotH);
                ctx.stroke();
                ctx.fillText(`${t}°`, x, height - 6);
            }
            const pts = root.points;
            if (pts.length === 0)
                return;
            const col = root.enabled ? Colors.blue : Colors.textDimmed;
            ctx.beginPath();
            ctx.moveTo(root.padL, root.py(pts[0].pwm));
            for (const p of pts)
                ctx.lineTo(root.px(p.t), root.py(p.pwm));
            ctx.lineTo(root.padL + root.plotW, root.py(pts[pts.length - 1].pwm));
            ctx.strokeStyle = col;
            ctx.lineWidth = 2;
            ctx.lineJoin = "round";
            ctx.stroke();
            ctx.lineTo(root.padL + root.plotW, root.padT + root.plotH);
            ctx.lineTo(root.padL, root.padT + root.plotH);
            ctx.closePath();
            ctx.fillStyle = Qt.alpha(col, 0.12);
            ctx.fill();
            for (let i = 0; i < pts.length; i++) {
                const hot = i === root.dragging || i === root.hovered;
                ctx.beginPath();
                ctx.arc(root.px(pts[i].t), root.py(pts[i].pwm), hot ? 6 : 4.5, 0, Math.PI * 2);
                ctx.fillStyle = hot ? Colors.textBright : Colors.background;
                ctx.fill();
                ctx.strokeStyle = col;
                ctx.lineWidth = 2;
                ctx.stroke();
            }
        }
    }

    Rectangle {
        visible: root.dragging >= 0 || root.hovered >= 0
        readonly property var p: root.points[root.dragging >= 0 ? root.dragging : root.hovered] ?? { t: 0, pwm: 0 }
        x: Math.max(0, Math.min(root.width - width, root.px(p.t) - width / 2))
        y: Math.max(0, root.py(p.pwm) - height - 12)
        width: tip.width + 12
        height: 20
        radius: 5
        color: Colors.background
        border.width: 1
        border.color: Colors.outline

        Text {
            id: tip
            anchors.centerIn: parent
            text: `${parent.p.t}°C · ${Math.round(parent.p.pwm / 255 * 100)}%`
            color: Colors.textBright
            font.pixelSize: 10
            font.family: Fonts.family
        }
    }

    MouseArea {
        anchors.fill: parent
        hoverEnabled: true
        enabled: root.enabled
        cursorShape: root.dragging >= 0 ? Qt.ClosedHandCursor : root.hovered >= 0 ? Qt.OpenHandCursor : Qt.ArrowCursor
        onPressed: mouse => root.dragging = root.nearest(mouse.x, mouse.y)
        onReleased: root.dragging = -1
        onPositionChanged: mouse => {
            if (root.dragging >= 0)
                root.move(root.dragging, mouse.x, mouse.y);
            else
                root.hovered = root.nearest(mouse.x, mouse.y);
        }
        onExited: root.hovered = -1
    }
}
