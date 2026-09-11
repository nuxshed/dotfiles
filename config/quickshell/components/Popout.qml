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

        hideTimer.stop()
        visible = true
        wrapper.opacity = 1
        wrapper.scale = 1
        watchdog.restart()
        root.opened()
    }

    function hide() {
        watchdog.stop()
        wrapper.opacity = 0
        wrapper.scale = 0.94
        hideTimer.restart()
    }

    Item {
        id: wrapper
        anchors.fill: parent
        opacity: 0
        scale: 0.94
        transformOrigin: Item.Left

        HoverHandler {
            onHoveredChanged: root.hovered = hovered
        }

        RoundCorner {
            corner: RoundCorner.CornerEnum.BottomLeft
            size: root.notch
            color: root.surface
        }

        Rectangle {
            id: card
            y: root.notch
            width: parent.width
            height: root.contentHeight
            color: root.surface
            radius: root.notch + 6
            topLeftRadius: 0
            bottomLeftRadius: 0

            Item {
                id: container
                anchors.fill: parent
            }
        }

        RoundCorner {
            y: card.y + card.height
            corner: RoundCorner.CornerEnum.TopLeft
            size: root.notch
            color: root.surface
        }

        Behavior on opacity {
            NumberAnimation { duration: 180; easing.type: Easing.OutCubic }
        }

        Behavior on scale {
            NumberAnimation { duration: 180; easing.type: Easing.OutCubic }
        }
    }

    Timer {
        id: hideTimer
        interval: 180
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
