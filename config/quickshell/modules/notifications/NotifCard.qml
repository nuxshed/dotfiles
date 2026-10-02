pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Widgets
import "../../components"
import "../../config"
import "../../services"

Item {
    id: root

    required property var notif
    property bool popup: false
    property bool expanded: false
    property bool replying: false

    readonly property bool hovered: hover.hovered
    readonly property bool hasImage: (notif?.image ?? "").length > 0
    readonly property bool canExpand: body.truncated || expanded

    signal tapped

    function source(path: string): string {
        if (path.length === 0)
            return "";
        if (path.startsWith("/"))
            return "file://" + path;
        return path.includes("://") ? path : Quickshell.iconPath(path, true);
    }

    function dismiss(): void {
        swipeOut.to = bg.x < 0 ? -width - 20 : width + 20;
        swipeOut.start();
    }

    function setReplying(on: bool): void {
        replying = on;
        Notifications.replying = on;
        if (on)
            Qt.callLater(() => replyInput.forceActiveFocus());
    }

    implicitHeight: bg.height

    Component.onDestruction: {
        if (replying)
            Notifications.replying = false;
    }

    HoverHandler {
        id: hover
    }

    Rectangle {
        id: bg

        width: root.width
        height: layout.implicitHeight + 20
        radius: root.popup ? 14 : 12
        color: root.popup ? Colors.background : root.hovered ? Colors.surfaceActive : Colors.surface
        border.width: root.popup ? 1 : 0
        border.color: root.notif?.critical ? Colors.red : Colors.border
        opacity: 1 - Math.min(0.7, Math.abs(x) / width)
        clip: true

        Behavior on color {
            ColorAnimation {
                duration: 150
            }
        }

        Behavior on height {
            Anim {
                duration: 220
            }
        }

        Behavior on x {
            enabled: !drag.drag.active && !swipeOut.running

            Anim {
                duration: 260
            }
        }

        NumberAnimation {
            id: swipeOut

            target: bg
            property: "x"
            duration: 180
            easing.type: Easing.InCubic
            onFinished: root.notif?.dismiss()
        }

        MouseArea {
            id: drag

            anchors.fill: parent
            acceptedButtons: Qt.LeftButton | Qt.MiddleButton
            cursorShape: Qt.PointingHandCursor
            drag.target: bg
            drag.axis: Drag.XAxis
            drag.threshold: 6

            onClicked: event => {
                if (event.button === Qt.MiddleButton)
                    root.dismiss();
                else if (!root.popup)
                    root.tapped();
                else if (root.notif?.hasDefault)
                    root.notif.activate();
                else if (root.canExpand)
                    root.expanded = !root.expanded;
            }

            onReleased: {
                if (Math.abs(bg.x) > root.width * 0.3)
                    root.dismiss();
                else
                    bg.x = 0;
            }
        }

        RowLayout {
            id: layout

            x: 10
            y: 10
            width: parent.width - 20
            spacing: 10

            Item {
                Layout.alignment: Qt.AlignTop
                Layout.topMargin: 1
                implicitWidth: 30
                implicitHeight: 30

                Rectangle {
                    anchors.fill: parent
                    radius: width / 2
                    color: root.notif?.critical ? Colors.red : Colors.subtle
                    visible: !avatar.visible

                    Image {
                        anchors.centerIn: parent
                        width: 18
                        height: 18
                        source: root.source(root.notif?.icon ?? "")
                        sourceSize: Qt.size(36, 36)
                        asynchronous: true
                        visible: status === Image.Ready
                    }

                    MaterialIcon {
                        anchors.centerIn: parent
                        text: root.notif?.critical ? "priority_high" : "notifications"
                        size: 15
                        color: root.notif?.critical ? Colors.background : Colors.textDimmed
                        visible: (root.notif?.icon ?? "").length === 0
                    }
                }

                ClippingRectangle {
                    id: avatar

                    anchors.fill: parent
                    radius: width / 2
                    color: Colors.subtle
                    visible: root.hasImage && picture.status !== Image.Error

                    Image {
                        id: picture

                        anchors.fill: parent
                        source: root.hasImage ? root.source(root.notif.image) : ""
                        fillMode: Image.PreserveAspectCrop
                        sourceSize: Qt.size(60, 60)
                        asynchronous: true
                    }
                }

                Rectangle {
                    anchors.right: parent.right
                    anchors.bottom: parent.bottom
                    anchors.margins: -2
                    width: 14
                    height: 14
                    radius: 7
                    color: bg.color
                    visible: avatar.visible && badge.status === Image.Ready

                    Image {
                        id: badge

                        anchors.centerIn: parent
                        width: 10
                        height: 10
                        source: root.source(root.notif?.icon ?? "")
                        sourceSize: Qt.size(20, 20)
                        asynchronous: true
                    }
                }
            }

            ColumnLayout {
                Layout.fillWidth: true
                spacing: 1

                RowLayout {
                    Layout.fillWidth: true
                    Layout.preferredHeight: 16
                    spacing: 4

                    Text {
                        Layout.fillWidth: true
                        text: root.notif?.summary || root.notif?.appName || ""
                        color: Colors.textBright
                        font.pixelSize: 11
                        font.family: Fonts.family
                        font.weight: Font.Medium
                        elide: Text.ElideRight
                    }

                    Text {
                        text: root.popup ? `${root.notif?.appName ?? ""} · ${root.notif?.timeStr ?? ""}` : root.notif?.timeStr ?? ""
                        color: Colors.textMuted
                        font.pixelSize: 9
                        font.family: Fonts.family
                        elide: Text.ElideRight
                        Layout.maximumWidth: 110
                        visible: !root.hovered
                    }

                    IconButton {
                        icon: "expand_more"
                        rotation: root.expanded ? 180 : 0
                        visible: root.canExpand && root.hovered
                        onClicked: root.expanded = !root.expanded

                        Behavior on rotation {
                            Anim {}
                        }
                    }

                    IconButton {
                        icon: "close"
                        visible: root.hovered
                        onClicked: root.dismiss()
                    }
                }

                Text {
                    id: body

                    Layout.fillWidth: true
                    visible: text.length > 0
                    text: root.notif?.body ?? ""
                    textFormat: Text.StyledText
                    color: Colors.textDimmed
                    linkColor: Colors.primary
                    font.pixelSize: 10
                    font.family: Fonts.family
                    wrapMode: Text.Wrap
                    maximumLineCount: root.expanded ? 16 : 2
                    elide: Text.ElideRight
                    lineHeight: 1.1
                    onLinkActivated: link => Qt.openUrlExternally(link)

                    HoverHandler {
                        cursorShape: body.hoveredLink ? Qt.PointingHandCursor : undefined
                    }
                }

                Flow {
                    Layout.fillWidth: true
                    Layout.topMargin: 6
                    spacing: 4
                    visible: actionRepeater.count > 0 || ((root.notif?.hasReply ?? false) && !root.replying)

                    Repeater {
                        id: actionRepeater

                        model: root.notif?.actions ?? []

                        Pill {
                            required property var modelData

                            text: modelData.text
                            onClicked: root.notif?.invoke(modelData.id)
                        }
                    }

                    Pill {
                        visible: (root.notif?.hasReply ?? false) && !root.replying
                        icon: "reply"
                        text: "Reply"
                        onClicked: root.setReplying(true)
                    }
                }

                Rectangle {
                    Layout.fillWidth: true
                    Layout.topMargin: 6
                    Layout.preferredHeight: 28
                    visible: root.replying
                    radius: 14
                    color: root.popup ? Colors.surface : Colors.background
                    border.width: 1
                    border.color: replyInput.activeFocus ? Colors.primary : Colors.border

                    TextInput {
                        id: replyInput

                        anchors.fill: parent
                        anchors.leftMargin: 12
                        anchors.rightMargin: 32
                        verticalAlignment: TextInput.AlignVCenter
                        clip: true
                        color: Colors.textBright
                        selectionColor: Colors.primaryContainer
                        font.pixelSize: 10
                        font.family: Fonts.family

                        onAccepted: {
                            if (text.trim().length > 0)
                                root.notif?.reply(text);
                            root.setReplying(false);
                        }

                        Keys.onEscapePressed: root.setReplying(false)

                        Text {
                            anchors.verticalCenter: parent.verticalCenter
                            text: root.notif?.replyHint ?? ""
                            color: Colors.textMuted
                            font: replyInput.font
                            visible: replyInput.text.length === 0
                        }
                    }

                    IconButton {
                        anchors.right: parent.right
                        anchors.rightMargin: 5
                        anchors.verticalCenter: parent.verticalCenter
                        icon: "send"
                        tint: replyInput.text.length > 0 ? Colors.primary : Colors.textMuted
                        onClicked: replyInput.accepted()
                    }
                }
            }
        }

        Rectangle {
            anchors.bottom: parent.bottom
            anchors.bottomMargin: 1
            anchors.left: parent.left
            anchors.leftMargin: bg.radius
            height: 1.5
            radius: 1
            width: (parent.width - bg.radius * 2) * (root.notif?.progress ?? 0)
            color: Colors.primary
            opacity: 0.35
            visible: root.popup && !(root.notif?.critical ?? true)
        }
    }

    component IconButton: Rectangle {
        id: btn

        property string icon
        property color tint: Colors.textDimmed

        signal clicked

        implicitWidth: 18
        implicitHeight: 18
        radius: 9
        color: btnMouse.containsMouse ? Colors.subtle : "transparent"

        Behavior on color {
            ColorAnimation {
                duration: 120
            }
        }

        MaterialIcon {
            anchors.centerIn: parent
            text: btn.icon
            size: 13
            color: btn.tint
        }

        MouseArea {
            id: btnMouse

            anchors.fill: parent
            hoverEnabled: true
            cursorShape: Qt.PointingHandCursor
            onClicked: btn.clicked()
        }
    }

    component Pill: Rectangle {
        id: pill

        property string icon
        property alias text: label.text

        signal clicked

        implicitWidth: row.implicitWidth + 18
        implicitHeight: 22
        radius: 11
        color: pillMouse.containsMouse ? Colors.subtle : root.popup ? Colors.surface : Colors.surfaceActive

        Behavior on color {
            ColorAnimation {
                duration: 120
            }
        }

        Row {
            id: row

            anchors.centerIn: parent
            spacing: 4

            MaterialIcon {
                anchors.verticalCenter: parent.verticalCenter
                text: pill.icon
                size: 12
                visible: pill.icon.length > 0
            }

            Text {
                id: label

                anchors.verticalCenter: parent.verticalCenter
                color: Colors.text
                font.pixelSize: 10
                font.family: Fonts.family
                font.weight: Font.Medium
            }
        }

        MouseArea {
            id: pillMouse

            anchors.fill: parent
            hoverEnabled: true
            cursorShape: Qt.PointingHandCursor
            onClicked: pill.clicked()
        }
    }
}
