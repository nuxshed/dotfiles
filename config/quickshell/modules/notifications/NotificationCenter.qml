pragma ComponentBehavior: Bound

import Quickshell
import Quickshell.Hyprland
import Quickshell.Wayland
import QtQuick
import "../../components"
import "../../config"
import "../../services"

Variants {
    model: Quickshell.screens

    PanelWindow {
        id: win

        required property var modelData

        readonly property bool isActive: Hyprland.focusedMonitor?.name === modelData.name
        readonly property bool expanded: (hovered || Notifications.open) && !Capture.busy
        readonly property bool inside: triggerHover.hovered || cardHover.hovered
        readonly property int margin: 10
        readonly property int islandHeight: 28
        readonly property int centerWidth: 360
        readonly property int popupWidth: 320
        readonly property int maxBody: Math.round(modelData.height * 0.7)
        readonly property int bodyHeight: Notifications.groups.length > 0 ? Math.min(maxBody, groups.contentHeight) : 120
        readonly property int centerHeight: 48 + bodyHeight + 12
        readonly property var bezier: [0.16, 1, 0.3, 1, 1, 1]

        property bool hovered: false

        screen: modelData
        visible: isActive && !Capture.busy
        color: "transparent"
        exclusionMode: ExclusionMode.Ignore
        implicitWidth: centerWidth + margin * 2

        WlrLayershell.layer: WlrLayer.Overlay
        WlrLayershell.namespace: "qs:notifications"
        WlrLayershell.keyboardFocus: Notifications.open || Notifications.replying ? WlrKeyboardFocus.Exclusive : WlrKeyboardFocus.None

        anchors {
            top: true
            bottom: true
            right: true
        }

        mask: Region {
            Region {
                item: trigger
            }

            Region {
                item: popupArea
            }
        }

        onInsideChanged: {
            if (inside) {
                unhover.stop();
                if (!hovered)
                    dwell.restart();
            } else {
                dwell.stop();
                unhover.restart();
            }
        }

        onExpandedChanged: {
            if (expanded) {
                Notifications.hidePopups();
                keys.forceActiveFocus();
            } else {
                Notifications.markRead();
            }
        }

        HyprlandFocusGrab {
            id: grab

            windows: [win]
            onCleared: Notifications.open = false
        }

        Timer {
            interval: 80
            running: Notifications.open && win.isActive
            onTriggered: grab.active = true
        }

        Connections {
            target: Notifications

            function onOpenChanged(): void {
                if (!Notifications.open)
                    grab.active = false;
            }

            function onReplyingChanged(): void {
                if (!Notifications.replying && !win.inside)
                    unhover.restart();
            }
        }

        Timer {
            id: dwell

            interval: 110
            onTriggered: win.hovered = true
        }

        Timer {
            id: unhover

            interval: 220
            onTriggered: {
                if (!Notifications.replying)
                    win.hovered = false;
            }
        }

        Item {
            id: keys

            anchors.fill: parent
            focus: true

            Keys.onEscapePressed: {
                win.hovered = false;
                Notifications.open = false;
            }
        }

        Item {
            id: trigger

            anchors.top: parent.top
            anchors.right: parent.right
            width: win.expanded ? card.width + win.margin * 2 : Math.max(220, card.width + 120)
            height: win.expanded ? card.height + win.margin * 2 : win.margin + win.islandHeight + 18

            HoverHandler {
                id: triggerHover
            }

            MouseArea {
                anchors.fill: parent
                enabled: !win.expanded
                onClicked: Notifications.open = true
            }
        }

        Rectangle {
            id: card

            property real w: win.expanded ? win.centerWidth : Math.max(win.islandHeight + 12, island.implicitWidth + 22)
            property real h: win.expanded ? win.centerHeight : win.islandHeight

            anchors.top: parent.top
            anchors.topMargin: win.margin
            anchors.right: parent.right
            anchors.rightMargin: win.margin
            width: Math.round(w)
            height: Math.round(h)
            radius: win.expanded ? 18 : win.islandHeight / 2
            color: Colors.background
            border.width: 1
            border.color: Colors.border
            clip: true

            Behavior on w {
                NumberAnimation {
                    duration: 340
                    easing.type: Easing.BezierSpline
                    easing.bezierCurve: win.bezier
                }
            }

            Behavior on h {
                NumberAnimation {
                    duration: 340
                    easing.type: Easing.BezierSpline
                    easing.bezierCurve: win.bezier
                }
            }

            Behavior on radius {
                NumberAnimation {
                    duration: 240
                }
            }

            HoverHandler {
                id: cardHover
            }

            MouseArea {
                anchors.fill: parent
                onClicked: {
                    if (!win.expanded)
                        Notifications.open = true;
                }
            }

            Row {
                id: island

                anchors.horizontalCenter: parent.horizontalCenter
                anchors.verticalCenter: parent.top
                anchors.verticalCenterOffset: win.islandHeight / 2
                spacing: 6
                opacity: win.expanded ? 0 : 1

                Behavior on opacity {
                    NumberAnimation {
                        duration: 120
                    }
                }

                MaterialIcon {
                    anchors.verticalCenter: parent.verticalCenter
                    visible: Notifications.dnd || Notifications.unread.length === 0
                    text: Notifications.dnd ? "notifications_off" : "notifications_none"
                    size: 14
                    color: Colors.textMuted
                }

                Row {
                    anchors.verticalCenter: parent.verticalCenter
                    spacing: -5
                    visible: Notifications.unread.length > 0

                    Repeater {
                        model: ScriptModel {
                            values: Notifications.unreadApps.slice(0, 3)
                        }

                        Rectangle {
                            id: chip

                            required property var modelData
                            required property int index

                            z: -index
                            width: 18
                            height: 18
                            radius: 9
                            color: Colors.surfaceActive
                            border.width: 2
                            border.color: Colors.background

                            Image {
                                anchors.centerIn: parent
                                width: 12
                                height: 12
                                source: {
                                    const icon = chip.modelData?.icon ?? "";
                                    if (icon.length === 0)
                                        return "";
                                    if (icon.startsWith("/"))
                                        return "file://" + icon;
                                    return icon.includes("://") ? icon : Quickshell.iconPath(icon, true);
                                }
                                sourceSize: Qt.size(24, 24)
                                asynchronous: true
                                visible: status === Image.Ready
                            }

                            MaterialIcon {
                                anchors.centerIn: parent
                                text: "notifications"
                                size: 11
                                visible: (chip.modelData?.icon ?? "").length === 0
                            }
                        }
                    }
                }

                Text {
                    anchors.verticalCenter: parent.verticalCenter
                    visible: Notifications.unread.length > 0
                    text: Notifications.unread.length
                    color: Colors.textBright
                    font.pixelSize: 11
                    font.family: Fonts.family
                    font.weight: Font.Medium
                }
            }

            Item {
                id: center

                width: win.centerWidth
                height: win.centerHeight
                anchors.right: parent.right
                opacity: win.expanded ? 1 : 0
                visible: opacity > 0

                Behavior on opacity {
                    NumberAnimation {
                        duration: win.expanded ? 220 : 90
                    }
                }

                Row {
                    x: 16
                    y: 12
                    height: 24
                    spacing: 6

                    Text {
                        anchors.verticalCenter: parent.verticalCenter
                        text: "Notifications"
                        color: Colors.textBright
                        font.pixelSize: 13
                        font.family: Fonts.family
                        font.weight: Font.Medium
                    }

                    Text {
                        anchors.verticalCenter: parent.verticalCenter
                        visible: Notifications.all.length > 0
                        text: Notifications.all.length
                        color: Colors.textMuted
                        font.pixelSize: 11
                        font.family: Fonts.family
                    }
                }

                Row {
                    anchors.right: parent.right
                    anchors.rightMargin: 12
                    y: 12
                    height: 24
                    spacing: 4

                    HeaderButton {
                        icon: Notifications.dnd ? "notifications_off" : "notifications_none"
                        active: Notifications.dnd
                        onClicked: Notifications.dnd = !Notifications.dnd
                    }

                    HeaderButton {
                        visible: Notifications.all.length > 0
                        icon: "clear_all"
                        onClicked: Notifications.clear()
                    }
                }

                Column {
                    anchors.horizontalCenter: parent.horizontalCenter
                    y: 48 + (win.bodyHeight - height) / 2
                    spacing: 8
                    visible: Notifications.groups.length === 0

                    MaterialIcon {
                        anchors.horizontalCenter: parent.horizontalCenter
                        text: "notifications_none"
                        size: 24
                        color: Colors.textMuted
                    }

                    Text {
                        anchors.horizontalCenter: parent.horizontalCenter
                        text: "You're all caught up"
                        color: Colors.textMuted
                        font.pixelSize: 10
                        font.family: Fonts.family
                    }
                }

                ListView {
                    id: groups

                    x: 12
                    y: 48
                    width: parent.width - 24
                    height: win.bodyHeight
                    spacing: 12
                    clip: true
                    boundsBehavior: Flickable.StopAtBounds

                    model: ScriptModel {
                        values: Notifications.groups
                    }

                    delegate: NotifGroup {
                        width: groups.width
                    }

                    add: Transition {
                        NumberAnimation {
                            property: "opacity"
                            from: 0
                            to: 1
                            duration: 200
                        }
                    }

                    remove: Transition {
                        NumberAnimation {
                            property: "opacity"
                            to: 0
                            duration: 150
                        }
                    }

                    displaced: Transition {
                        NumberAnimation {
                            property: "y"
                            duration: 260
                            easing.type: Easing.OutCubic
                        }
                        NumberAnimation {
                            property: "opacity"
                            to: 1
                            duration: 150
                        }
                    }

                    NumberAnimation {
                        id: scroll

                        target: groups
                        property: "contentY"
                        duration: 200
                        easing.type: Easing.OutCubic
                    }

                    WheelHandler {
                        acceptedDevices: PointerDevice.Mouse | PointerDevice.TouchPad

                        onWheel: event => {
                            const delta = event.pixelDelta.y !== 0 ? event.pixelDelta.y * 4 : event.angleDelta.y / 120 * 240;
                            const from = scroll.running ? scroll.to : groups.contentY;
                            scroll.to = Math.max(0, Math.min(groups.contentHeight - groups.height, from - delta));
                            scroll.restart();
                        }
                    }
                }
            }
        }

        Item {
            id: popupArea

            readonly property bool held: popupHover.hovered

            anchors.right: parent.right
            anchors.rightMargin: win.margin
            y: card.y + win.islandHeight + 8
            width: win.popupWidth
            height: win.expanded ? 0 : popups.contentHeight
            visible: !win.expanded

            onHeldChanged: held ? Notifications.hold() : Notifications.release()

            Component.onDestruction: {
                if (held)
                    Notifications.release();
            }

            HoverHandler {
                id: popupHover
            }

            ListView {
                id: popups

                width: parent.width
                height: contentHeight
                spacing: 8
                interactive: false

                model: ScriptModel {
                    values: Notifications.popups
                }

                delegate: NotifCard {
                    required property var modelData

                    width: popups.width
                    notif: modelData
                    popup: true
                }

                add: Transition {
                    NumberAnimation {
                        property: "x"
                        from: 80
                        to: 0
                        duration: 320
                        easing.type: Easing.BezierSpline
                        easing.bezierCurve: win.bezier
                    }
                    NumberAnimation {
                        property: "opacity"
                        from: 0
                        to: 1
                        duration: 180
                    }
                }

                remove: Transition {
                    NumberAnimation {
                        property: "opacity"
                        to: 0
                        duration: 160
                    }
                    NumberAnimation {
                        property: "x"
                        to: 60
                        duration: 200
                        easing.type: Easing.InCubic
                    }
                }

                displaced: Transition {
                    NumberAnimation {
                        property: "y"
                        duration: 280
                        easing.type: Easing.BezierSpline
                        easing.bezierCurve: win.bezier
                    }
                    NumberAnimation {
                        properties: "opacity"
                        to: 1
                        duration: 150
                    }
                    NumberAnimation {
                        property: "x"
                        to: 0
                        duration: 200
                    }
                }
            }
        }
    }

    component HeaderButton: Rectangle {
        id: hb

        property string icon
        property bool active: false

        signal clicked

        implicitWidth: 24
        implicitHeight: 24
        radius: 12
        color: active ? Colors.primary : hbMouse.containsMouse ? Colors.surfaceActive : Colors.surface

        Behavior on color {
            ColorAnimation {
                duration: 150
            }
        }

        MaterialIcon {
            anchors.centerIn: parent
            text: hb.icon
            size: 13
            color: hb.active ? Colors.primaryText : Colors.textDimmed
        }

        MouseArea {
            id: hbMouse

            anchors.fill: parent
            hoverEnabled: true
            cursorShape: Qt.PointingHandCursor
            onClicked: hb.clicked()
        }
    }
}
