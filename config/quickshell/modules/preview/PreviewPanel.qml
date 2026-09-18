pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import Quickshell
import "../../components"
import "../../config"
import "../../services"

FloatingWindow {
    id: root

    property real natW: image.sourceSize.width
    property real natH: image.sourceSize.height
    readonly property bool rotated: Preview.rotation % 180 !== 0
    readonly property real rotW: rotated ? natH : natW
    readonly property real rotH: rotated ? natW : natH
    readonly property real fitScale: {
        if (rotW <= 0 || rotH <= 0)
            return 1;
        return Math.min((viewport.width - 48) / rotW, (viewport.height - 48) / rotH, 8);
    }
    readonly property real scale: Preview.fit ? fitScale : Preview.zoom
    readonly property real paperW: rotW * scale
    readonly property real paperH: rotH * scale

    visible: Preview.open
    implicitWidth: 1100
    implicitHeight: 760
    minimumSize.width: 560
    minimumSize.height: 420
    color: "transparent"
    title: (Preview.dirty ? "● " : "") + (Preview.title.length > 0 ? Preview.title : "Preview")

    onVisibleChanged: if (visible) keyScope.forceActiveFocus()

    WindowChrome { titleHeight: 52 }

    function rotateShapes(cw: bool): void {
        Preview.shapes = Preview.shapes.map(sh => {
            const r = Object.assign({}, sh);
            const map = p => cw ? ({ x: 1 - p.y, y: p.x }) : ({ x: p.y, y: 1 - p.x });
            if (sh.tool === "pen" || sh.tool === "highlight") {
                r.points = sh.points.map(map);
            } else {
                const a = map({ x: sh.x1, y: sh.y1 });
                const b = map({ x: sh.x2, y: sh.y2 });
                r.x1 = a.x; r.y1 = a.y; r.x2 = b.x; r.y2 = b.y;
            }
            return r;
        });
    }

    function flatten(): void {
        const ow = Math.round(Preview.cropRect ? root.rotW * Preview.cropRect.w : root.rotW);
        const oh = Math.round(Preview.cropRect ? root.rotH * Preview.cropRect.h : root.rotH);
        const tmp = `/tmp/qs-preview-${Date.now()}.png`;
        // Grab at (at least) native size; DPR scaling is normalised away by the
        // resize to exact pixels in Preview.delivered().
        cropClip.grabToImage(result => {
            result.saveToFile(tmp);
            Preview.delivered(tmp, ow, oh);
        }, Qt.size(ow, oh));
    }

    Connections {
        target: Preview

        function onFlattenRequested() {
            root.flatten();
        }

        function onFitRequested() {
            Qt.callLater(() => { flick.contentX = (flick.contentWidth - flick.width) / 2; flick.contentY = (flick.contentHeight - flick.height) / 2; });
        }

        function onSaveAsRequested(tmp, dir, name, w, h) {
            Files.pickSave(dir, name, dest => Preview.savedAs(tmp, dest, w, h));
        }

        function onGenerationChanged() {
            Preview.zoom = 1;
        }
    }

    Item {
        id: keyScope
        anchors.fill: parent
        focus: true


        Keys.onPressed: event => {
            const ctrl = event.modifiers & Qt.ControlModifier;
            if (event.key === Qt.Key_Escape) {
                if (Preview.cropping)
                    Preview.cancelCrop();
                else if (Preview.tool !== "none")
                    Preview.setTool(Preview.tool);
                else
                    Preview.close();
            } else if (ctrl && event.key === Qt.Key_S) {
                Preview.save();
            } else if (ctrl && event.key === Qt.Key_Z) {
                Preview.undo();
            } else if (ctrl && (event.key === Qt.Key_Plus || event.key === Qt.Key_Equal)) {
                Preview.zoomBy(1.25);
            } else if (ctrl && event.key === Qt.Key_Minus) {
                Preview.zoomBy(0.8);
            } else if (ctrl && event.key === Qt.Key_0) {
                Preview.zoomFit();
            } else if (ctrl && event.key === Qt.Key_1) {
                Preview.zoomActual();
            } else if (event.key === Qt.Key_BracketLeft) {
                root.rotateShapes(false);
                Preview.rotate(-90);
            } else if (event.key === Qt.Key_BracketRight) {
                root.rotateShapes(true);
                Preview.rotate(90);
            } else {
                return;
            }
            event.accepted = true;
        }
    }

    ColumnLayout {
        anchors.fill: parent
        spacing: 0

        // ---- toolbar ----
        Rectangle {
            Layout.fillWidth: true
            Layout.preferredHeight: 52
            color: Colors.surface
            topLeftRadius: 16
            topRightRadius: 16

            RowLayout {
                anchors.fill: parent
                anchors.leftMargin: 12
                anchors.rightMargin: 12
                spacing: 6

                PreviewButton { icon: "zoom_out"; onClicked: Preview.zoomBy(0.8) }
                Text {
                    Layout.preferredWidth: 46
                    text: Math.round(root.scale * 100) + "%"
                    color: Colors.textDimmed
                    font.pixelSize: 11
                    font.family: Fonts.family
                    horizontalAlignment: Text.AlignHCenter
                }
                PreviewButton { icon: "zoom_in"; onClicked: Preview.zoomBy(1.25) }
                PreviewButton { icon: "fullscreen"; active: Preview.fit; onClicked: Preview.zoomFit() }
                PreviewButton { icon: "crop_original"; onClicked: Preview.zoomActual() }

                Divider {}

                PreviewButton { icon: "rotate_left"; onClicked: { root.rotateShapes(false); Preview.rotate(-90); } }
                PreviewButton { icon: "rotate_right"; onClicked: { root.rotateShapes(true); Preview.rotate(90); } }
                PreviewButton { icon: "crop"; active: Preview.cropping; onClicked: Preview.cropping ? Preview.cancelCrop() : Preview.startCrop() }

                Divider {}

                PreviewButton { icon: "edit"; active: Preview.tool === "pen"; onClicked: Preview.setTool("pen") }
                PreviewButton { icon: "highlight"; active: Preview.tool === "highlight"; onClicked: Preview.setTool("highlight") }
                PreviewButton { icon: "crop_square"; active: Preview.tool === "rect"; onClicked: Preview.setTool("rect") }
                PreviewButton { icon: "call_made"; active: Preview.tool === "arrow"; onClicked: Preview.setTool("arrow") }
                PreviewButton { icon: "title"; active: Preview.tool === "text"; onClicked: Preview.setTool("text") }

                Row {
                    Layout.leftMargin: 4
                    spacing: 5
                    visible: Preview.tool !== "none"

                    Repeater {
                        model: [Colors.red, Colors.yellow, Colors.green, Colors.blue, Colors.textBright]

                        Rectangle {
                            required property var modelData
                            width: 16
                            height: 16
                            radius: 8
                            color: modelData
                            border.width: Preview.stroke == modelData ? 2 : 0
                            border.color: Colors.textBright
                            anchors.verticalCenter: parent.verticalCenter

                            MouseArea {
                                anchors.fill: parent
                                cursorShape: Qt.PointingHandCursor
                                onClicked: Preview.stroke = parent.color
                            }
                        }
                    }
                }

                PreviewButton { icon: "undo"; enabled: Preview.shapes.length > 0; onClicked: Preview.undo() }

                Item { Layout.fillWidth: true }

                Text {
                    visible: Preview.notice.length > 0
                    text: Preview.notice
                    color: Colors.textMuted
                    font.pixelSize: 11
                    font.family: Fonts.family
                }

                PreviewButton { icon: "content_copy"; onClicked: Preview.copyToClipboard() }
                PreviewButton { icon: "add_to_photos"; onClicked: Preview.saveCopy() }
                PreviewButton { icon: "save"; enabled: Preview.dirty; onClicked: Preview.save() }
                PreviewButton { icon: "close"; onClicked: Preview.close() }
            }
        }

        Rectangle { Layout.fillWidth: true; Layout.preferredHeight: 1; color: Colors.border }

        // ---- viewport ----
        Rectangle {
            id: viewport

            Layout.fillWidth: true
            Layout.fillHeight: true
            color: Colors.backgroundDeep
            bottomLeftRadius: Preview.cropping ? 0 : 16
            bottomRightRadius: Preview.cropping ? 0 : 16
            clip: true

            Flickable {
                id: flick

                anchors.fill: parent
                contentWidth: Math.max(width, paper.width + 48)
                contentHeight: Math.max(height, paper.height + 48)
                interactive: !Preview.cropping && Preview.tool === "none"
                boundsBehavior: Flickable.StopAtBounds
                clip: true

                Item {
                    id: cropClip

                    x: Math.max(24, (flick.contentWidth - width) / 2)
                    y: Math.max(24, (flick.contentHeight - height) / 2)
                    width: Preview.cropRect ? root.paperW * Preview.cropRect.w : root.paperW
                    height: Preview.cropRect ? root.paperH * Preview.cropRect.h : root.paperH
                    // crop bar reset shown when a crop is applied
                    clip: true

                    Item {
                        id: paper

                        x: Preview.cropRect ? -root.paperW * Preview.cropRect.x : 0
                        y: Preview.cropRect ? -root.paperH * Preview.cropRect.y : 0
                        width: root.paperW
                        height: root.paperH

                        Image {
                            id: image
                            anchors.centerIn: parent
                            width: root.rotated ? parent.height : parent.width
                            height: root.rotated ? parent.width : parent.height
                            rotation: Preview.rotation
                            source: Preview.path.length > 0 ? "file://" + Preview.path : ""
                            cache: false
                            smooth: true
                            fillMode: Image.Stretch
                        }

                        Canvas {
                            id: canvas
                            anchors.fill: parent
                            renderStrategy: Canvas.Immediate

                            function pt(p) {
                                return { x: p.x * width, y: p.y * height };
                            }

                            function drawShape(ctx, shape) {
                                ctx.strokeStyle = shape.color;
                                ctx.fillStyle = shape.color;
                                ctx.lineCap = "round";
                                ctx.lineJoin = "round";
                                const lw = Math.max(2, root.paperW * 0.004);
                                if (shape.tool === "highlight") {
                                    ctx.globalAlpha = 0.35;
                                    ctx.lineWidth = lw * 4;
                                } else {
                                    ctx.globalAlpha = 1;
                                    ctx.lineWidth = lw;
                                }
                                if (shape.tool === "pen" || shape.tool === "highlight") {
                                    ctx.beginPath();
                                    shape.points.forEach((p, i) => {
                                        const c = canvas.pt(p);
                                        if (i === 0) ctx.moveTo(c.x, c.y); else ctx.lineTo(c.x, c.y);
                                    });
                                    ctx.stroke();
                                } else if (shape.tool === "rect") {
                                    const a = canvas.pt({ x: shape.x1, y: shape.y1 });
                                    const b = canvas.pt({ x: shape.x2, y: shape.y2 });
                                    ctx.strokeRect(a.x, a.y, b.x - a.x, b.y - a.y);
                                } else if (shape.tool === "arrow") {
                                    const a = canvas.pt({ x: shape.x1, y: shape.y1 });
                                    const b = canvas.pt({ x: shape.x2, y: shape.y2 });
                                    const ang = Math.atan2(b.y - a.y, b.x - a.x);
                                    const head = lw * 5;
                                    ctx.beginPath();
                                    ctx.moveTo(a.x, a.y);
                                    ctx.lineTo(b.x, b.y);
                                    ctx.stroke();
                                    ctx.beginPath();
                                    ctx.moveTo(b.x, b.y);
                                    ctx.lineTo(b.x - head * Math.cos(ang - Math.PI / 7), b.y - head * Math.sin(ang - Math.PI / 7));
                                    ctx.lineTo(b.x - head * Math.cos(ang + Math.PI / 7), b.y - head * Math.sin(ang + Math.PI / 7));
                                    ctx.closePath();
                                    ctx.fill();
                                } else if (shape.tool === "text") {
                                    const a = canvas.pt({ x: shape.x1, y: shape.y1 });
                                    ctx.font = `bold ${Math.max(12, root.paperW * 0.03)}px sans-serif`;
                                    ctx.textBaseline = "top";
                                    ctx.fillText(shape.text, a.x, a.y);
                                }
                            }

                            onPaint: {
                                const ctx = getContext("2d");
                                ctx.reset();
                                ctx.clearRect(0, 0, width, height);
                                for (const s of Preview.shapes)
                                    drawShape(ctx, s);
                                if (Preview.draft)
                                    drawShape(ctx, Preview.draft);
                            }

                            Connections {
                                target: Preview
                                function onShapesChanged() { canvas.requestPaint(); }
                                function onDraftChanged() { canvas.requestPaint(); }
                            }
                            onWidthChanged: requestPaint()
                            onHeightChanged: requestPaint()
                        }

                        MouseArea {
                            anchors.fill: parent
                            enabled: Preview.tool !== "none" && !Preview.cropping
                            cursorShape: Qt.CrossCursor
                            preventStealing: true

                            function nx(v) { return Math.max(0, Math.min(1, v / width)); }
                            function ny(v) { return Math.max(0, Math.min(1, v / height)); }

                            onPressed: mouse => {
                                if (Preview.tool === "text") {
                                    textEntry.nx = nx(mouse.x);
                                    textEntry.ny = ny(mouse.y);
                                    textEntry.x = mouse.x;
                                    textEntry.y = mouse.y - 12;
                                    textEntry.text = "";
                                    Preview.editing = true;
                                    textEntry.forceActiveFocus();
                                    return;
                                }
                                const t = Preview.tool;
                                Preview.draft = (t === "pen" || t === "highlight")
                                    ? { tool: t, color: String(Preview.stroke), points: [{ x: nx(mouse.x), y: ny(mouse.y) }] }
                                    : { tool: t, color: String(Preview.stroke), x1: nx(mouse.x), y1: ny(mouse.y), x2: nx(mouse.x), y2: ny(mouse.y) };
                            }

                            onPositionChanged: mouse => {
                                if (!Preview.draft || !pressed)
                                    return;
                                const d = Preview.draft;
                                if (d.tool === "pen" || d.tool === "highlight")
                                    d.points.push({ x: nx(mouse.x), y: ny(mouse.y) });
                                else {
                                    d.x2 = nx(mouse.x);
                                    d.y2 = ny(mouse.y);
                                }
                                Preview.draft = d;
                                canvas.requestPaint();
                            }

                            onReleased: {
                                if (!Preview.draft)
                                    return;
                                Preview.addShape(Preview.draft);
                                Preview.draft = null;
                            }
                        }

                        TextInput {
                            id: textEntry
                            property real nx: 0
                            property real ny: 0
                            visible: Preview.editing
                            color: Preview.stroke
                            font.pixelSize: Math.max(12, root.paperW * 0.03)
                            font.family: Fonts.family
                            font.weight: Font.Medium

                            onAccepted: {
                                if (text.length > 0)
                                    Preview.addShape({ tool: "text", color: String(Preview.stroke), x1: nx, y1: ny, text: text });
                                Preview.editing = false;
                            }
                            Keys.onEscapePressed: Preview.editing = false
                        }
                    }
                }

                // crop overlay
                CropOverlay {
                    anchors.fill: cropClip
                    visible: Preview.cropping
                    paperW: root.paperW
                    paperH: root.paperH
                }
            }

            Text {
                anchors.centerIn: parent
                visible: image.status !== Image.Ready
                text: image.status === Image.Error ? "Cannot load image" : "Loading…"
                color: Colors.textMuted
                font.pixelSize: 13
                font.family: Fonts.family
            }

            WheelHandler {
                target: null
                onWheel: event => {
                    if (event.modifiers & Qt.ControlModifier)
                        Preview.zoomBy(event.angleDelta.y > 0 ? 1.12 : 0.89);
                    else
                        event.accepted = false;
                }
            }
        }

        // crop action bar
        Rectangle {
            Layout.fillWidth: true
            Layout.preferredHeight: Preview.cropping ? 46 : 0
            visible: Preview.cropping
            color: Colors.surface
            bottomLeftRadius: 16
            bottomRightRadius: 16

            RowLayout {
                anchors.fill: parent
                anchors.leftMargin: 14
                anchors.rightMargin: 12
                spacing: 12

                Text {
                    text: "Drag to select a crop region"
                    color: Colors.textDimmed
                    font.pixelSize: 12
                    font.family: Fonts.family
                    Layout.fillWidth: true
                }

                PreviewTextButton {
                    text: "Reset"
                    visible: Preview.cropRect !== null
                    onClicked: Preview.resetCrop()
                }

                PreviewTextButton { text: "Cancel"; onClicked: Preview.cancelCrop() }
                PreviewTextButton {
                    text: "Apply"
                    primary: true
                    enabled: Preview.cropSel !== null
                    onClicked: Preview.applyCrop()
                }
            }
        }
    }

    component Divider: Rectangle {
        Layout.leftMargin: 4
        Layout.rightMargin: 4
        Layout.preferredWidth: 1
        Layout.preferredHeight: 22
        color: Colors.border
    }
}
