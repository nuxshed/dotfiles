import Quickshell
import Quickshell.Wayland
import QtQuick
import "../config"

PanelWindow {
    id: root

    property int cardWidth: 320
    property int cardHeight: 200
    property real posX: 120
    property real posY: 120

    default property alias content: holder.data

    color: "transparent"
    exclusionMode: ExclusionMode.Ignore

    WlrLayershell.layer: WlrLayer.Overlay
    WlrLayershell.namespace: "qs:floating-card"

    anchors {
        top: true
        bottom: true
        left: true
        right: true
    }

    mask: Region {
        id: cardRegion
        item: card
    }

    Rectangle {
        id: card

        width: root.cardWidth
        height: root.cardHeight
        radius: 10
        color: Colors.background
        border.width: 1
        border.color: Colors.border
        clip: true

        Component.onCompleted: {
            x = root.posX;
            y = root.posY;
        }

        onXChanged: cardRegion.changed()
        onYChanged: cardRegion.changed()

        MouseArea {
            anchors.fill: parent
            cursorShape: drag.active ? Qt.ClosedHandCursor : Qt.OpenHandCursor

            drag.target: card
            drag.minimumX: 0
            drag.maximumX: root.width - card.width
            drag.minimumY: 0
            drag.maximumY: root.height - card.height
            drag.threshold: 2
        }

        Item {
            id: holder
            anchors.fill: parent
        }
    }
}
