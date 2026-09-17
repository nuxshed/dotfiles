import Quickshell
import Quickshell.Wayland
import QtQuick
import "../../config"
import "../../services"

Variants {
    model: Quickshell.screens

    PanelWindow {
        id: win

        required property var modelData

        property string image: ""
        property real sx: 0
        property real sy: 0
        property real ex: 0
        property real ey: 0
        property bool dragging: false
        property var hoverWin: null

        readonly property real pxRatio: frozen.sourceSize.width > 0 ? frozen.sourceSize.width / width : 1
        readonly property rect drag: Qt.rect(Math.min(sx, ex), Math.min(sy, ey), Math.abs(ex - sx), Math.abs(ey - sy))
        readonly property bool hasDrag: drag.width > 4 && drag.height > 4
        readonly property rect sel: hasDrag ? drag : windowRect(hoverWin)

        function windowRect(w) {
            if (!w)
                return Qt.rect(0, 0, 0, 0)
            return Qt.rect(w.at[0] - modelData.x, w.at[1] - modelData.y, w.size[0], w.size[1])
        }

        function windowAt(x, y) {
            const gx = x + modelData.x
            const gy = y + modelData.y
            let best = null
            for (const w of HyprlandData.windowList) {
                if (w.hidden || !w.mapped || w.workspace.id < 0)
                    continue
                if (gx < w.at[0] || gy < w.at[1] || gx > w.at[0] + w.size[0] || gy > w.at[1] + w.size[1])
                    continue
                if (!best || w.size[0] * w.size[1] < best.size[0] * best.size[1])
                    best = w
            }
            return best
        }

        function close() {
            image = ""
            dragging = false
            sx = sy = ex = ey = 0
            hoverWin = null
        }

        function confirm() {
            const r = sel
            if (r.width < 4 || r.height < 4) {
                cancel()
                return
            }
            const x = Math.round(r.x * pxRatio)
            const y = Math.round(r.y * pxRatio)
            const w = Math.round(r.width * pxRatio)
            const h = Math.round(r.height * pxRatio)
            close()
            Capture.crop(x, y, w, h)
        }

        function cancel() {
            close()
            Capture.cancel()
        }

        screen: modelData
        visible: image !== ""
        exclusionMode: ExclusionMode.Ignore
        color: "transparent"

        WlrLayershell.layer: WlrLayer.Overlay
        WlrLayershell.namespace: "qs:region"
        WlrLayershell.keyboardFocus: visible ? WlrKeyboardFocus.Exclusive : WlrKeyboardFocus.None

        anchors {
            top: true
            bottom: true
            left: true
            right: true
        }

        Connections {
            target: Capture

            function onRegionReady(screen, path) {
                if (screen !== win.modelData)
                    return
                HyprlandData.updateWindowList()
                win.image = path
            }
        }

        Image {
            id: frozen
            anchors.fill: parent
            source: win.image ? "file://" + win.image : ""
            cache: false
            fillMode: Image.Stretch
        }

        Rectangle {
            anchors.fill: parent
            color: "#000000"
            opacity: 0.5
        }

        Item {
            x: win.sel.x
            y: win.sel.y
            width: win.sel.width
            height: win.sel.height
            clip: true
            visible: win.sel.width > 0

            Image {
                x: -parent.x
                y: -parent.y
                width: win.width
                height: win.height
                source: frozen.source
                cache: false
                fillMode: Image.Stretch
            }
        }

        Rectangle {
            x: win.sel.x
            y: win.sel.y
            width: win.sel.width
            height: win.sel.height
            visible: win.sel.width > 0
            color: "transparent"
            border.width: 1
            border.color: Colors.textBright
        }

        Rectangle {
            x: Math.min(win.width - width - 8, Math.max(8, win.sel.x))
            y: win.sel.y > 34 ? win.sel.y - 30 : win.sel.y + win.sel.height + 8
            visible: win.sel.width > 0
            width: label.implicitWidth + 16
            height: 24
            radius: 6
            color: Colors.background

            Text {
                id: label
                anchors.centerIn: parent
                text: `${Math.round(win.sel.width * win.pxRatio)} × ${Math.round(win.sel.height * win.pxRatio)}`
                color: Colors.text
                font.pixelSize: 12
                font.family: Fonts.family
            }
        }

        MouseArea {
            anchors.fill: parent
            hoverEnabled: true
            cursorShape: Qt.CrossCursor
            acceptedButtons: Qt.LeftButton | Qt.RightButton

            onPressed: mouse => {
                if (mouse.button === Qt.RightButton) {
                    win.cancel()
                    return
                }
                win.dragging = true
                win.sx = win.ex = mouse.x
                win.sy = win.ey = mouse.y
            }

            onPositionChanged: mouse => {
                if (win.dragging) {
                    win.ex = mouse.x
                    win.ey = mouse.y
                } else {
                    win.hoverWin = win.windowAt(mouse.x, mouse.y)
                }
            }

            onReleased: {
                win.dragging = false
                win.confirm()
            }
        }

        Item {
            anchors.fill: parent
            focus: true
            Keys.onEscapePressed: win.cancel()
        }
    }
}
