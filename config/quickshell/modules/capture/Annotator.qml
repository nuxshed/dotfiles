import Quickshell
import Quickshell.Wayland
import Quickshell.Hyprland
import QtQuick
import QtQuick.Layouts
import "../../components"
import "../../config"
import "../../services"

Variants {
    model: Quickshell.screens

    PanelWindow {
        id: win

        required property var modelData

        readonly property bool isActive: Hyprland.focusedMonitor?.name === modelData.name

        property string image: ""
        property string tool: "pen"
        property color stroke: Colors.red
        property var shapes: []
        property var draft: null
        property bool editing: false

        function close() {
            const old = image
            image = ""
            shapes = []
            draft = null
            editing = false
            if (old)
                Quickshell.execDetached(["bash", "-c", `rm -f '${old}'`])
        }

        function undo() {
            if (shapes.length > 0)
                shapes = shapes.slice(0, -1)
            canvas.requestPaint()
        }

        function commit(then) {
            surface.grabToImage(result => {
                const out = `/tmp/qs-capture/annotated-${Date.now()}.png`
                result.saveToFile(out)
                then(out)
                win.close()
            }, Qt.size(source.sourceSize.width, source.sourceSize.height))
        }

        function copyToClipboard() {
            commit(out => Quickshell.execDetached(["bash", "-c", `wl-copy -t image/png < '${out}'; sleep 1; rm -f '${out}'`]))
        }

        function save() {
            commit(out => {
                const dest = `${Capture.shotDir}/Screenshot_${Capture.stamp()}.png`
                Quickshell.execDetached(["bash", "-c", `mkdir -p '${Capture.shotDir}' && mv '${out}' '${dest}' && wl-copy -t image/png < '${dest}'`])
                Capture.notify("Screenshot saved", dest.replace(Capture.home, "~"), dest)
            })
        }

        screen: modelData
        visible: isActive && image !== ""
        exclusionMode: ExclusionMode.Ignore
        color: "transparent"

        WlrLayershell.layer: WlrLayer.Overlay
        WlrLayershell.namespace: "qs:annotator"
        WlrLayershell.keyboardFocus: visible ? WlrKeyboardFocus.Exclusive : WlrKeyboardFocus.None

        anchors {
            top: true
            bottom: true
            left: true
            right: true
        }

        Connections {
            target: Capture

            function onAnnotateReady(path) {
                win.shapes = []
                win.image = path
            }
        }

        Rectangle {
            anchors.fill: parent
            color: "#000000"
            opacity: 0.75

            MouseArea {
                anchors.fill: parent
                onClicked: win.close()
            }
        }

        ColumnLayout {
            anchors.centerIn: parent
            spacing: 14

            Item {
                id: surface

                readonly property real maxWidth: win.width * 0.82
                readonly property real maxHeight: win.height * 0.74
                readonly property real ratio: source.sourceSize.height > 0 ? source.sourceSize.width / source.sourceSize.height : 1

                Layout.alignment: Qt.AlignHCenter
                implicitWidth: Math.min(maxWidth, maxHeight * ratio)
                implicitHeight: Math.min(maxHeight, maxWidth / ratio)

                Image {
                    id: source
                    anchors.fill: parent
                    source: win.image ? "file://" + win.image : ""
                    cache: false
                    fillMode: Image.Stretch
                }

                Canvas {
                    id: canvas

                    anchors.fill: parent

                    function drawShape(ctx, shape) {
                        ctx.strokeStyle = shape.color
                        ctx.fillStyle = shape.color
                        ctx.lineWidth = 3
                        ctx.lineCap = "round"
                        ctx.lineJoin = "round"

                        if (shape.tool === "pen") {
                            ctx.beginPath()
                            for (let i = 0; i < shape.points.length; i++) {
                                const p = shape.points[i]
                                if (i === 0)
                                    ctx.moveTo(p.x, p.y)
                                else
                                    ctx.lineTo(p.x, p.y)
                            }
                            ctx.stroke()
                        } else if (shape.tool === "rect") {
                            ctx.strokeRect(shape.x1, shape.y1, shape.x2 - shape.x1, shape.y2 - shape.y1)
                        } else if (shape.tool === "arrow") {
                            const angle = Math.atan2(shape.y2 - shape.y1, shape.x2 - shape.x1)
                            const head = 14
                            ctx.beginPath()
                            ctx.moveTo(shape.x1, shape.y1)
                            ctx.lineTo(shape.x2, shape.y2)
                            ctx.stroke()
                            ctx.beginPath()
                            ctx.moveTo(shape.x2, shape.y2)
                            ctx.lineTo(shape.x2 - head * Math.cos(angle - Math.PI / 7), shape.y2 - head * Math.sin(angle - Math.PI / 7))
                            ctx.lineTo(shape.x2 - head * Math.cos(angle + Math.PI / 7), shape.y2 - head * Math.sin(angle + Math.PI / 7))
                            ctx.closePath()
                            ctx.fill()
                        } else if (shape.tool === "text") {
                            ctx.font = "bold 18px sans-serif"
                            ctx.fillText(shape.text, shape.x1, shape.y1)
                        }
                    }

                    onPaint: {
                        const ctx = getContext("2d")
                        ctx.clearRect(0, 0, width, height)
                        for (const shape of win.shapes)
                            drawShape(ctx, shape)
                        if (win.draft)
                            drawShape(ctx, win.draft)
                    }
                }

                MouseArea {
                    anchors.fill: parent
                    cursorShape: Qt.CrossCursor

                    onPressed: mouse => {
                        if (win.tool === "text") {
                            textEntry.x = mouse.x
                            textEntry.y = mouse.y - 12
                            textEntry.text = ""
                            win.editing = true
                            textEntry.forceActiveFocus()
                            return
                        }
                        win.draft = win.tool === "pen" ? {
                            tool: "pen",
                            color: win.stroke,
                            points: [{
                                x: mouse.x,
                                y: mouse.y
                            }]
                        } : {
                            tool: win.tool,
                            color: win.stroke,
                            x1: mouse.x,
                            y1: mouse.y,
                            x2: mouse.x,
                            y2: mouse.y
                        }
                        canvas.requestPaint()
                    }

                    onPositionChanged: mouse => {
                        if (!win.draft || !pressed)
                            return
                        if (win.draft.tool === "pen")
                            win.draft.points.push({
                                x: mouse.x,
                                y: mouse.y
                            })
                        else {
                            win.draft.x2 = mouse.x
                            win.draft.y2 = mouse.y
                        }
                        canvas.requestPaint()
                    }

                    onReleased: {
                        if (!win.draft)
                            return
                        win.shapes = win.shapes.concat([win.draft])
                        win.draft = null
                        canvas.requestPaint()
                    }
                }

                TextInput {
                    id: textEntry

                    visible: win.editing
                    color: win.stroke
                    font.pixelSize: 18
                    font.family: Fonts.family
                    font.weight: Font.Medium

                    onAccepted: {
                        if (text.length > 0)
                            win.shapes = win.shapes.concat([{
                                tool: "text",
                                color: win.stroke,
                                x1: x,
                                y1: y + 14,
                                text: text
                            }])
                        win.editing = false
                        canvas.requestPaint()
                    }
                }
            }

            Rectangle {
                Layout.alignment: Qt.AlignHCenter
                Layout.preferredWidth: tools.implicitWidth + 24
                Layout.preferredHeight: 48
                radius: 24
                color: Colors.background

                RowLayout {
                    id: tools
                    anchors.centerIn: parent
                    spacing: 4

                    ToolChip {
                        icon: "edit"
                        active: win.tool === "pen"
                        onActivated: win.tool = "pen"
                    }

                    ToolChip {
                        icon: "crop_square"
                        active: win.tool === "rect"
                        onActivated: win.tool = "rect"
                    }

                    ToolChip {
                        icon: "arrow_forward"
                        active: win.tool === "arrow"
                        onActivated: win.tool = "arrow"
                    }

                    ToolChip {
                        icon: "title"
                        active: win.tool === "text"
                        onActivated: win.tool = "text"
                    }

                    Rectangle {
                        Layout.preferredWidth: 1
                        Layout.preferredHeight: 22
                        Layout.leftMargin: 5
                        Layout.rightMargin: 5
                        color: Colors.border
                    }

                    Repeater {
                        model: [Colors.red, Colors.yellow, Colors.green, Colors.blue, Colors.textBright]

                        Rectangle {
                            required property var modelData

                            Layout.preferredWidth: 18
                            Layout.preferredHeight: 18
                            radius: 9
                            color: modelData
                            border.width: win.stroke == modelData ? 2 : 0
                            border.color: Colors.textBright

                            MouseArea {
                                anchors.fill: parent
                                cursorShape: Qt.PointingHandCursor
                                onClicked: win.stroke = parent.color
                            }
                        }
                    }

                    Rectangle {
                        Layout.preferredWidth: 1
                        Layout.preferredHeight: 22
                        Layout.leftMargin: 5
                        Layout.rightMargin: 5
                        color: Colors.border
                    }

                    ToolChip {
                        icon: "undo"
                        onActivated: win.undo()
                    }

                    ToolChip {
                        icon: "content_copy"
                        onActivated: win.copyToClipboard()
                    }

                    ToolChip {
                        icon: "save"
                        onActivated: win.save()
                    }

                    ToolChip {
                        icon: "close"
                        onActivated: win.close()
                    }
                }
            }
        }

        Item {
            anchors.fill: parent
            focus: true
            Keys.onEscapePressed: win.close()
        }

        component ToolChip: Rectangle {
            id: chip

            property string icon: ""
            property bool active: false

            signal activated

            Layout.preferredWidth: 34
            Layout.preferredHeight: 34
            radius: 17
            color: chip.active ? Colors.surfaceActive : (chipArea.containsMouse ? Colors.surface : "transparent")

            Behavior on color {
                ColorAnimation {
                    duration: 140
                }
            }

            MaterialIcon {
                anchors.centerIn: parent
                text: chip.icon
                size: 18
                color: chip.active ? Colors.textBright : Colors.textDimmed
            }

            MouseArea {
                id: chipArea
                anchors.fill: parent
                hoverEnabled: true
                cursorShape: Qt.PointingHandCursor
                onClicked: chip.activated()
            }
        }
    }
}
