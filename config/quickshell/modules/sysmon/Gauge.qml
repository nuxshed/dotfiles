import QtQuick
import "../../config"

Rectangle {
    id: root

    property string label: ""
    property real value: 0
    property real max: 100
    property string display: ""
    property string unit: ""
    property string sublabel: ""
    property color accent: Colors.textBright
    property bool available: true

    radius: 10
    color: Colors.surfaceActive
    border.width: 1
    border.color: Colors.outline
    implicitHeight: 150

    onValueChanged: canvas.requestPaint()
    onAccentChanged: canvas.requestPaint()
    onAvailableChanged: canvas.requestPaint()

    Canvas {
        id: canvas
        anchors.fill: parent
        anchors.margins: 12
        anchors.bottomMargin: 30
        onWidthChanged: requestPaint()
        onHeightChanged: requestPaint()

        onPaint: {
            const ctx = getContext("2d");
            ctx.clearRect(0, 0, width, height);
            const cx = width / 2, cy = height / 2 + 6;
            const r = Math.min(width, height) / 2 - 6;
            const start = Math.PI * 0.75, span = Math.PI * 1.5;
            ctx.lineCap = "round";
            ctx.lineWidth = 7;
            ctx.strokeStyle = Colors.subtle;
            ctx.beginPath();
            ctx.arc(cx, cy, r, start, start + span);
            ctx.stroke();
            if (!root.available)
                return;
            const f = Math.max(0, Math.min(1, root.value / root.max));
            if (f > 0) {
                ctx.strokeStyle = root.accent;
                ctx.beginPath();
                ctx.arc(cx, cy, r, start, start + span * f);
                ctx.stroke();
            }
        }
    }

    Column {
        anchors.centerIn: parent
        anchors.verticalCenterOffset: -6
        spacing: 0

        Text {
            anchors.horizontalCenter: parent.horizontalCenter
            text: root.available ? root.display : "—"
            color: Colors.textBright
            font.pixelSize: 22
            font.family: Fonts.family
            font.weight: Font.Medium
        }
        Text {
            anchors.horizontalCenter: parent.horizontalCenter
            text: root.available ? root.unit : ""
            color: Colors.textMuted
            font.pixelSize: 10
            font.family: Fonts.family
        }
    }

    Column {
        anchors.bottom: parent.bottom
        anchors.bottomMargin: 10
        anchors.horizontalCenter: parent.horizontalCenter
        spacing: 1

        Text {
            anchors.horizontalCenter: parent.horizontalCenter
            text: root.label
            color: Colors.text
            font.pixelSize: 11
            font.family: Fonts.family
            font.weight: Font.Medium
        }
        Text {
            anchors.horizontalCenter: parent.horizontalCenter
            text: root.sublabel
            color: Colors.textMuted
            font.pixelSize: 10
            font.family: Fonts.family
            visible: text.length > 0
        }
    }
}
