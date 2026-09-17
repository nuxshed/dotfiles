import QtQuick
import "../../config"
import "../../services"

Canvas {
    id: root

    property var series: []
    property real max: 100
    property bool grid: true
    property var fmt: null
    property real hoverX: -1
    property int span: SysMon.histLen

    onSeriesChanged: requestPaint()
    onGridChanged: requestPaint()
    onHoverXChanged: requestPaint()

    MouseArea {
        anchors.fill: parent
        hoverEnabled: true
        acceptedButtons: Qt.NoButton
        onPositionChanged: mouse => root.hoverX = mouse.x
        onExited: root.hoverX = -1
    }
    onWidthChanged: requestPaint()
    onHeightChanged: requestPaint()

    onPaint: {
        const ctx = getContext("2d");
        ctx.clearRect(0, 0, width, height);
        let m = root.max;
        if (m <= 0) {
            m = 1;
            for (const s of root.series)
                for (const v of s.data)
                    m = Math.max(m, v);
            m *= 1.15;
        }
        ctx.strokeStyle = Qt.alpha(Colors.textMuted, 0.12);
        ctx.lineWidth = 1;
        for (let i = 0; root.grid && i <= 2; i++) {
            const y = Math.round(height * i / 2) + 0.5;
            ctx.beginPath();
            ctx.moveTo(0, Math.min(y, height - 0.5));
            ctx.lineTo(width, Math.min(y, height - 0.5));
            ctx.stroke();
        }
        let n = root.span;
        for (const s of root.series)
            n = Math.max(n, s.data.length);
        const step = width / Math.max(1, n - 1);
        const yOf = v => height - 1 - Math.min(1, v / m) * (height - 2);
        for (const s of root.series) {
            const d = s.data;
            if (d.length < 2)
                continue;
            ctx.beginPath();
            for (let i = 0; i < d.length; i++) {
                const x = width - (d.length - 1 - i) * step;
                if (i === 0) ctx.moveTo(x, yOf(d[i])); else ctx.lineTo(x, yOf(d[i]));
            }
            ctx.strokeStyle = s.color;
            ctx.lineWidth = 1.5;
            ctx.lineJoin = "round";
            ctx.stroke();
            ctx.lineTo(width, height);
            ctx.lineTo(width - (d.length - 1) * step, height);
            ctx.closePath();
            const g = ctx.createLinearGradient(0, 0, 0, height);
            g.addColorStop(0, Qt.alpha(s.color, 0.22));
            g.addColorStop(1, Qt.alpha(s.color, 0.02));
            ctx.fillStyle = g;
            ctx.fill();
        }
        if (root.hoverX < 0)
            return;
        const f = root.fmt ?? (v => `${v.toFixed(0)}%`);
        const labels = [];
        let hx = -1;
        for (const s of root.series) {
            const d = s.data;
            if (d.length < 2)
                continue;
            const i = Math.round((root.hoverX - (width - (d.length - 1) * step)) / step);
            if (i < 0 || i >= d.length)
                continue;
            hx = width - (d.length - 1 - i) * step;
            labels.push({ text: f(d[i]), color: s.color, y: yOf(d[i]) });
        }
        if (hx < 0)
            return;
        ctx.font = "11px 'Google Sans Flex'";
        const tw = Math.max(...labels.map(l => ctx.measureText(l.text).width)) + 20;
        const th = labels.length * 16 + 6;
        const bx = hx + 10 + tw > width ? hx - 10 - tw : hx + 10;
        const by = Math.max(0, Math.min(height - th, labels[0].y - th / 2));
        ctx.fillStyle = Colors.background;
        ctx.strokeStyle = Colors.outline;
        ctx.beginPath();
        ctx.roundedRect(bx, by, tw, th, 5, 5);
        ctx.fill();
        ctx.stroke();
        labels.forEach((l, k) => {
            ctx.fillStyle = l.color;
            ctx.beginPath();
            ctx.arc(bx + 9, by + 11 + k * 16, 3, 0, Math.PI * 2);
            ctx.fill();
            ctx.fillStyle = Colors.textBright;
            ctx.textAlign = "left";
            ctx.fillText(l.text, bx + 17, by + 15 + k * 16);
        });
        for (const l of labels) {
            ctx.beginPath();
            ctx.arc(hx, l.y, 3.5, 0, Math.PI * 2);
            ctx.fillStyle = l.color;
            ctx.fill();
        }
    }
}
