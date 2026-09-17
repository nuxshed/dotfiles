import QtQuick
import "../../config"

Item {
    id: root

    required property real unit

    property date now: new Date()
    property bool placed: false
    property real vx: 90 * unit
    property real vy: 70 * unit
    property real last: Date.now()

    function step(): void {
        const t = Date.now();
        const dt = Math.min((t - root.last) / 1000, 0.05);
        root.last = t;

        if (root.width <= 0 || body.width <= 0)
            return;

        if (!root.placed) {
            body.x = Math.random() * (root.width - body.width);
            body.y = Math.random() * (root.height - body.height);
            root.placed = true;
            return;
        }

        let x = body.x + root.vx * dt;
        let y = body.y + root.vy * dt;

        if (x <= 0 || x + body.width >= root.width) {
            root.vx = -root.vx;
            x = Math.max(0, Math.min(x, root.width - body.width));
        }

        if (y <= 0 || y + body.height >= root.height) {
            root.vy = -root.vy;
            y = Math.max(0, Math.min(y, root.height - body.height));
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

    Item {
        id: body

        width: col.width
        height: col.height

        Column {
            id: col

            spacing: 0

            Text {
                id: time

                text: Qt.formatTime(root.now, "HH:mm")
                font.pixelSize: 120 * root.unit
                font.family: Fonts.family
                font.weight: Font.Black
                font.italic: true
                font.letterSpacing: -6 * root.unit
                color: Colors.textBright
            }

            Item {
                id: disc

                width: time.width * 1.08
                height: 34 * root.unit
                anchors.horizontalCenter: parent.horizontalCenter
                transform: Translate { y: -22 * root.unit }

                Rectangle {
                    anchors.centerIn: parent
                    width: disc.width
                    height: width
                    radius: width / 2
                    color: Colors.textBright
                    transform: Scale { origin.y: disc.width / 2; yScale: disc.height / disc.width }
                }

                Rectangle {
                    anchors.centerIn: parent
                    width: disc.width * 0.2
                    height: width
                    radius: width / 2
                    color: Colors.backgroundDeep
                    transform: Scale { origin.y: disc.width * 0.1; yScale: disc.height / disc.width * 1.6 }
                }
            }

            Text {
                width: parent.width
                horizontalAlignment: Text.AlignHCenter
                topPadding: 8 * root.unit
                transform: Translate { y: -22 * root.unit }
                text: Qt.formatDate(root.now, "ddd dd MMM").toUpperCase()
                font.pixelSize: 24 * root.unit
                font.family: Fonts.family
                font.weight: Font.Bold
                font.letterSpacing: 10 * root.unit
                color: Colors.textBright
            }
        }

        Repeater {
            model: Math.floor(body.height / 4)

            Rectangle {
                required property int index

                y: index * 4
                width: body.width
                height: 2
                color: Qt.rgba(0, 0, 0, 0.5)
            }
        }
    }
}
