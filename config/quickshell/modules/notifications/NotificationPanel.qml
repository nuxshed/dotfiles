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
    readonly property int headerHeight: 26
    readonly property int gap: 8
    readonly property int hotWidth: 56
    readonly property int hotHeight: 8
    readonly property int scrollStep: 560

    readonly property int maxListHeight: Math.max(180, Math.round(modelData.height * 0.7) - headerHeight - gap - padding * 2)
    readonly property int bodyHeight: Notifications.all.length > 0 ? Math.min(maxListHeight, Math.max(1, list.contentHeight)) : 52
    readonly property int fullHeight: padding * 2 + headerHeight + gap + bodyHeight
    readonly property int popupHeight: popups.height > 0 ? popups.height + padding * 2 : 0

    readonly property bool hovered: hotHover.hovered || bgHover.hovered
    readonly property bool holding: hovered || expanded

    property bool expanded: false

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
    implicitHeight: Math.max(hotHeight, bg.height + notch)

    onHoveredChanged: {
        if (hovered)
            closeTimer.stop();
        else if (expanded)
            closeTimer.restart();
    }

    onHoldingChanged: holding ? Notifications.hold() : Notifications.release()

    Component.onDestruction: {
        if (holding)
            Notifications.release();
    }

    mask: Region {
        id: region

        Region {
            item: bg
        }

        Region {
            x: root.width - root.hotWidth
            y: 0
            width: root.hotWidth
            height: root.hotHeight
        }
    }

    Timer {
        id: closeTimer

        interval: 180
        onTriggered: root.expanded = false
    }

    Rectangle {
        id: bg

        x: root.notch
        width: root.cardWidth + root.padding * 2
        height: root.expanded ? root.fullHeight : root.popupHeight

        color: Colors.background
        radius: 22
        topLeftRadius: 0
        topRightRadius: 0
        bottomRightRadius: 0
        clip: true

        onHeightChanged: region.changed()

        Behavior on height {
            Anim {
                duration: 250
            }
        }

        HoverHandler {
            id: bgHover
        }

        Column {
            id: popups

            x: root.padding
            y: root.padding
            width: root.cardWidth
            spacing: 6

            opacity: root.expanded ? 0 : 1
            visible: opacity > 0

            Behavior on opacity {
                Anim {
                    duration: 150
                }
            }

            Repeater {
                model: ScriptModel {
                    values: Notifications.popups
                }

                delegate: Item {
                    id: popupItem

                    required property var modelData

                    implicitWidth: root.cardWidth
                    implicitHeight: popupCard.implicitHeight

                    Notification {
                        id: popupCard

                        implicitWidth: root.cardWidth
                        notif: popupItem.modelData
                    }
                }
            }
        }

        Item {
            id: store

            x: root.padding
            y: root.padding
            width: root.cardWidth
            height: root.fullHeight - root.padding * 2

            opacity: root.expanded ? 1 : 0
            visible: opacity > 0

            Behavior on opacity {
                Anim {
                    duration: 150
                }
            }

            Text {
                anchors.left: parent.left
                anchors.leftMargin: 2
                anchors.verticalCenter: clearBtn.verticalCenter

                text: "Notifications"
                color: Colors.textBright
                font.pixelSize: 12
                font.bold: true
            }

            Rectangle {
                id: clearBtn

                anchors.right: parent.right
                anchors.top: parent.top

                width: 22
                height: 22
                radius: height / 2
                color: clearHover.containsMouse ? Colors.surfaceActive : Colors.surface
                visible: Notifications.all.length > 0

                Behavior on color {
                    ColorAnimation {
                        duration: 150
                    }
                }

                MaterialIcon {
                    anchors.centerIn: parent
                    text: "clear_all"
                    size: 14
                }

                MouseArea {
                    id: clearHover

                    anchors.fill: parent
                    hoverEnabled: true
                    cursorShape: Qt.PointingHandCursor
                    onClicked: Notifications.clear()
                }
            }

            Text {
                anchors.centerIn: parent
                text: "No notifications"
                color: Colors.textMuted
                font.pixelSize: 11
                visible: Notifications.all.length === 0
            }

            ListView {
                id: list

                anchors.left: parent.left
                anchors.right: parent.right
                anchors.top: parent.top
                anchors.topMargin: root.headerHeight + root.gap

                height: root.maxListHeight
                spacing: 6
                clip: true
                cacheBuffer: 400
                boundsBehavior: Flickable.StopAtBounds
                visible: Notifications.all.length > 0

                model: ScriptModel {
                    values: Notifications.all
                }

                NumberAnimation {
                    id: scrollAnim

                    target: list
                    property: "contentY"
                    duration: 200
                    easing.type: Easing.OutCubic
                }

                WheelHandler {
                    acceptedDevices: PointerDevice.Mouse | PointerDevice.TouchPad

                    onWheel: event => {
                        const delta = event.pixelDelta.y !== 0 ? event.pixelDelta.y * 4 : event.angleDelta.y / 120 * root.scrollStep;
                        const from = scrollAnim.running ? scrollAnim.to : list.contentY;
                        scrollAnim.to = Math.max(0, Math.min(list.contentHeight - list.height, from - delta));
                        scrollAnim.restart();
                    }
                }

                delegate: Item {
                    id: storeItem

                    required property var modelData

                    width: list.width
                    height: storeCard.implicitHeight

                    Notification {
                        id: storeCard

                        implicitWidth: list.width
                        popupMode: false
                        notif: storeItem.modelData
                    }
                }
            }
        }

        Rectangle {
            anchors.right: parent.right
            anchors.rightMargin: 5

            y: store.y + list.y + list.visibleArea.yPosition * list.height
            width: 3
            height: Math.max(24, list.visibleArea.heightRatio * list.height)
            radius: width / 2
            color: Colors.subtle

            visible: root.expanded && list.contentHeight > list.height
            opacity: list.moving ? 1 : 0.5

            Behavior on opacity {
                Anim {
                    duration: 150
                }
            }
        }
    }

    Item {
        x: root.width - root.hotWidth
        z: 1
        width: root.hotWidth
        height: root.hotHeight

        HoverHandler {
            id: hotHover

            onHoveredChanged: {
                if (hovered)
                    root.expanded = true;
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
