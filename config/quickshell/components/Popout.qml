import Quickshell
import QtQuick
import "../config"

PopupWindow {
    id: root

    property var panel: null
    property Item target: null
    property int notch: 18
    property int contentWidth: 280
    property int contentHeight: 200
    property color surface: Colors.background
    property bool hovered: false
    property bool shown: false
    readonly property int pill: 56
    readonly property var bezier: [0.2, 0.9, 0.3, 1, 1, 1]

    default property alias content: container.data

    signal opened()

    implicitWidth: contentWidth
    implicitHeight: contentHeight + notch * 2
    color: "transparent"
    visible: false

    function show(item) {
        if (!panel || !item)
            return

        target = item
        const centerY = item.mapToItem(null, 0, item.height / 2).y

        anchor.window = panel
        anchor.rect.x = panel.width
        anchor.rect.y = Math.max(0, Math.min(centerY - height / 2, panel.height - height))
        anchor.rect.width = 1
        anchor.rect.height = 1
        anchor.edges = Edges.Top | Edges.Left
        anchor.gravity = Edges.Bottom | Edges.Right

        const fresh = !root.shown
        hideTimer.stop()
        visible = true
        root.shown = true
        watchdog.restart()
        if (fresh)
            stagger()
        root.opened()
    }

    function hide() {
        watchdog.stop()
        root.shown = false
        hideTimer.restart()
    }

    function stagger() {
        const layout = container.children[0]
        if (!layout)
            return
        let i = 0
        for (const child of layout.children) {
            if (!child.visible || child.width <= 0 || child.height <= 0)
                continue
            child.opacity = 0
            popIn.createObject(root, { target: child, delay: 30 + i++ * 30 }).start()
        }
    }

    Component {
        id: popIn

        SequentialAnimation {
            id: anim

            required property Item target
            required property int delay

            onFinished: destroy()

            PauseAnimation {
                duration: anim.delay
            }

            ParallelAnimation {
                NumberAnimation {
                    target: anim.target
                    property: "opacity"
                    to: 1
                    duration: 120
                }
                NumberAnimation {
                    target: anim.target
                    property: "scale"
                    from: 0.94
                    to: 1
                    duration: 240
                    easing.type: Easing.OutBack
                    easing.overshoot: 1.4
                }
            }
        }
    }

    Item {
        id: wrapper

        anchors.fill: parent
        opacity: root.shown ? 1 : 0

        transform: Translate {
            x: root.shown ? 0 : -16

            Behavior on x {
                NumberAnimation {
                    duration: 220
                    easing.bezierCurve: root.bezier
                }
            }
        }

        Behavior on opacity {
            NumberAnimation { duration: 90 }
        }

        HoverHandler {
            onHoveredChanged: root.hovered = hovered
        }

        RoundCorner {
            y: card.y - root.notch
            corner: RoundCorner.CornerEnum.BottomLeft
            size: root.notch
            color: root.surface
        }

        Rectangle {
            id: card

            y: root.notch + (root.contentHeight - height) / 2
            width: root.shown ? parent.width : root.pill
            height: root.shown ? root.contentHeight : root.pill
            color: root.surface
            radius: root.notch + 6
            topLeftRadius: 0
            bottomLeftRadius: 0
            clip: true

            Behavior on width {
                NumberAnimation {
                    duration: 200
                    easing.bezierCurve: root.bezier
                }
            }

            Behavior on height {
                NumberAnimation {
                    duration: 230
                    easing.bezierCurve: root.bezier
                }
            }

            Item {
                id: container
                width: root.contentWidth
                height: root.contentHeight
            }
        }

        RoundCorner {
            y: card.y + card.height
            corner: RoundCorner.CornerEnum.TopLeft
            size: root.notch
            color: root.surface
        }
    }

    Timer {
        id: hideTimer
        interval: 120
        onTriggered: root.visible = false
    }

    Timer {
        id: watchdog

        property int misses: 0

        interval: 200
        repeat: true
        onRunningChanged: misses = 0
        onTriggered: {
            if (root.hovered || (root.target && root.target.isHovered))
                misses = 0
            else if (++misses >= 2)
                root.hide()
        }
    }
}
