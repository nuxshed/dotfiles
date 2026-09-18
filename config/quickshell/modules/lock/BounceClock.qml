import QtQuick
import QtQuick.Effects
import QtQuick.Shapes
import "../../config"

Item {
    id: root

    required property real unit

    property date now: new Date()
    property bool placed: false
    property real vx: 90 * unit
    property real vy: 70 * unit
    property real last: Date.now()
    property real flicker: 1
    property real roll: 0

    readonly property real lift: 16 * unit
    readonly property real visTop: time.baselineOffset + timeMetrics.tightBoundingRect.y
    readonly property real visBottom: date.y - lift + date.baselineOffset + dateMetrics.tightBoundingRect.y + dateMetrics.tightBoundingRect.height

    function step(): void {
        const t = Date.now();
        const dt = Math.min((t - root.last) / 1000, 0.05);
        root.last = t;
        root.flicker = 0.94 + Math.random() * 0.06;
        root.roll = (root.roll + dt * 6) % 4;

        if (root.width <= 0 || body.width <= 0)
            return;

        if (!root.placed) {
            body.x = Math.random() * (root.width - body.width);
            body.y = Math.random() * (root.height - root.visBottom);
            root.placed = true;
            return;
        }

        let x = body.x + root.vx * dt;
        let y = body.y + root.vy * dt;

        if (x <= 0 || x + body.width >= root.width) {
            root.vx = -root.vx;
            x = Math.max(0, Math.min(x, root.width - body.width));
        }

        if (y + root.visTop <= 0 || y + root.visBottom >= root.height) {
            root.vy = -root.vy;
            y = Math.max(-root.visTop, Math.min(y, root.height - root.visBottom));
        }

        body.x = x;
        body.y = y;
    }

    Timer {
        interval: 16
        running: true
        repeat: true
        onTriggered: root.step()
    }

    Timer {
        interval: 1000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: root.now = new Date()
    }

    TextMetrics {
        id: timeMetrics
        font: time.font
        text: time.text
    }

    TextMetrics {
        id: dateMetrics
        font: date.font
        text: date.text
    }

    Item {
        id: body

        width: col.width
        height: col.height
        opacity: root.flicker

        Column {
            id: col

            visible: false
            spacing: 0

            Text {
                id: time

                text: Qt.formatTime(root.now, "HH:mm")
                font.pixelSize: 96 * root.unit
                font.family: Fonts.family
                font.variableAxes: ({ "wdth": 75, "wght": 1000, "slnt": -10 })
                font.letterSpacing: -2 * root.unit
                color: Colors.textBright
            }

            Shape {
                id: disc

                width: time.width * 1.12
                height: 26 * root.unit
                anchors.horizontalCenter: parent.horizontalCenter
                transform: Translate { y: -root.lift }
                preferredRendererType: Shape.CurveRenderer

                ShapePath {
                    fillColor: Colors.textBright
                    strokeWidth: -1
                    fillRule: ShapePath.OddEvenFill

                    PathAngleArc { centerX: disc.width / 2; centerY: disc.height / 2; radiusX: disc.width / 2; radiusY: disc.height / 2; startAngle: 0; sweepAngle: 360 }
                    PathAngleArc { centerX: disc.width / 2; centerY: disc.height / 2; radiusX: disc.width * 0.1; radiusY: disc.height * 0.3; startAngle: 0; sweepAngle: 360 }
                }
            }

            Text {
                id: date

                width: parent.width
                horizontalAlignment: Text.AlignHCenter
                topPadding: 6 * root.unit
                transform: Translate { y: -root.lift }
                text: Qt.formatDate(root.now, "ddd dd MMM").toUpperCase()
                font.pixelSize: 17 * root.unit
                font.family: Fonts.family
                font.variableAxes: ({ "wdth": 100, "wght": 800 })
                font.letterSpacing: 8 * root.unit
                color: Colors.textBright
            }
        }

        MultiEffect {
            source: col
            anchors.fill: col
            blurEnabled: true
            blur: 1
            blurMax: 48
            brightness: 0.4
            opacity: 0.55
        }

        MultiEffect {
            source: col
            anchors.fill: col
            anchors.leftMargin: -2 * root.unit
            anchors.rightMargin: 2 * root.unit
            colorization: 1
            colorizationColor: Colors.red
            opacity: 0.35
        }

        MultiEffect {
            source: col
            anchors.fill: col
            anchors.leftMargin: 2 * root.unit
            anchors.rightMargin: -2 * root.unit
            colorization: 1
            colorizationColor: Colors.cyan
            opacity: 0.35
        }

        MultiEffect {
            source: col
            anchors.fill: col
        }

        Repeater {
            model: Math.floor(body.height / 4) + 1

            Rectangle {
                required property int index

                y: index * 4 - 4 + root.roll
                width: body.width
                height: 2
                color: Qt.rgba(0, 0, 0, 0.5)
            }
        }
    }
}
