pragma ComponentBehavior: Bound

import Quickshell
import Quickshell.Wayland
import QtQuick
import "../../components"
import "../../config"
import "../../services"

PanelWindow {
    id: root

    required property var modelData

    readonly property int padding: 12
    readonly property int notch: 16
    readonly property int cardWidth: 320

    screen: modelData

    anchors {
        top: true
        right: true
    }

    WlrLayershell.layer: WlrLayer.Overlay
    WlrLayershell.namespace: "notifications"
    WlrLayershell.exclusionMode: ExclusionMode.Ignore

    color: "transparent"

    implicitWidth: notch + cardWidth + padding * 2
    implicitHeight: bg.height > 0 ? bg.height + notch : 1

    mask: Region {
        item: bg
    }

    Rectangle {
        id: bg

        x: root.notch
        width: root.cardWidth + root.padding * 2
        height: column.height > 0 ? column.height + root.padding * 2 : 0

        color: Colors.background
        radius: 22
        topLeftRadius: 0
        topRightRadius: 0
        bottomRightRadius: 0
        clip: true

        Column {
            id: column

            x: root.padding
            y: root.padding
            width: root.cardWidth
            spacing: 6

            Repeater {
                model: ScriptModel {
                    values: Notifications.popups
                }

                delegate: Item {
                    id: wrapper

                    required property var modelData

                    implicitWidth: root.cardWidth
                    implicitHeight: card.implicitHeight

                    Notification {
                        id: card

                        implicitWidth: root.cardWidth
                        notif: wrapper.modelData
                    }
                }
            }
        }
    }

    RoundCorner {
        corner: RoundCorner.CornerEnum.TopRight
        size: root.notch
        color: Colors.background
        visible: bg.height > 0
    }

    RoundCorner {
        x: root.width - root.notch
        y: bg.height
        corner: RoundCorner.CornerEnum.TopRight
        size: root.notch
        color: Colors.background
        visible: bg.height > 0
    }
}
