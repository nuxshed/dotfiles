pragma ComponentBehavior: Bound

import QtQuick
import Quickshell
import "../../components"
import "../../config"

Rectangle {
    id: root

    required property var notif

    property bool popupMode: true

    readonly property int actionCount: notif?.actions.length ?? 0

    readonly property bool hasImage: (notif?.image.length ?? 0) > 0
    readonly property bool hasAppIcon: (notif?.appIcon.length ?? 0) > 0
    readonly property bool dismissing: popupMode ? !(notif?.popup ?? false) : (notif?.closing ?? false)
    readonly property int nonAnimHeight: Math.max(image.height, summary.implicitHeight + (expanded ? 6 + appName.height + body.height + actions.height + actions.anchors.topMargin : bodyPreview.height)) + inner.anchors.margins * 2

    property bool expanded: false
    property bool opened: false
    property bool collapsed: false

    color: notif?.critical ? Colors.surfaceActive : Colors.surface
    radius: 14
    clip: true

    implicitHeight: inner.implicitHeight

    x: popupMode ? implicitWidth : 0
    Component.onCompleted: {
        x = 0;
        opened = true;
    }

    onDismissingChanged: {
        if (!dismissing)
            return;
        slideOut.to = x >= 0 ? implicitWidth : -implicitWidth;
        exitAnim.start();
    }

    SequentialAnimation {
        id: exitAnim

        NumberAnimation {
            id: slideOut

            target: root
            property: "x"
            duration: 250
            easing.type: Easing.BezierSpline
            easing.bezierCurve: [0.3, 0, 0.8, 0.15, 1, 1]
        }

        PropertyAction {
            target: root
            property: "collapsed"
            value: true
        }
    }

    Behavior on x {
        enabled: !mouse.drag.active

        Anim {
            duration: 350
            easing.bezierCurve: [0.05, 0.7, 0.1, 1, 1, 1]
        }
    }

    MouseArea {
        id: mouse

        property real startY

        anchors.fill: parent
        hoverEnabled: true
        preventStealing: root.popupMode
        acceptedButtons: Qt.LeftButton | Qt.MiddleButton
        cursorShape: pressed ? Qt.ClosedHandCursor : Qt.ArrowCursor

        drag.target: root
        drag.axis: Drag.XAxis

        onPressed: event => {
            startY = event.y;
            if (event.button === Qt.MiddleButton)
                root.notif?.dismiss();
        }

        onPositionChanged: event => {
            if (pressed && root.popupMode) {
                const diffY = event.y - startY;
                if (Math.abs(diffY) > 20)
                    root.expanded = diffY > 0;
            }
        }

        onReleased: {
            if (Math.abs(root.x) < root.implicitWidth * 0.3)
                root.x = 0;
            else if (root.popupMode)
                root.notif.popup = false;
            else
                root.notif?.dismiss();
        }
    }

    Item {
        id: inner

        anchors.left: parent.left
        anchors.right: parent.right
        anchors.top: parent.top
        anchors.margins: 11

        implicitHeight: root.opened && !root.collapsed ? root.nonAnimHeight : 0

        Behavior on implicitHeight {
            Anim {
                duration: 250
            }
        }

        Rectangle {
            id: image

            anchors.left: parent.left
            anchors.top: parent.top

            width: 34
            height: 34
            radius: width / 2
            clip: true
            color: root.notif?.critical ? Colors.red : Colors.subtle

            Image {
                anchors.fill: parent
                source: root.notif?.image ?? ""
                fillMode: Image.PreserveAspectCrop
                sourceSize.width: 34
                sourceSize.height: 34
                visible: root.hasImage
                asynchronous: true
            }

            MaterialIcon {
                anchors.centerIn: parent
                text: "info"
                size: 18
                color: Colors.textBright
                visible: !root.hasImage && !appIcon.visible
            }
        }

        Rectangle {
            id: appIcon

            anchors.horizontalCenter: root.hasImage ? undefined : image.horizontalCenter
            anchors.verticalCenter: root.hasImage ? undefined : image.verticalCenter
            anchors.right: root.hasImage ? image.right : undefined
            anchors.bottom: root.hasImage ? image.bottom : undefined

            width: root.hasImage ? 16 : 34
            height: width
            radius: width / 2
            color: root.hasImage ? (root.notif?.critical ? Colors.red : Colors.subtle) : "transparent"
            visible: root.hasAppIcon && iconImage.status === Image.Ready

            Image {
                id: iconImage

                anchors.centerIn: parent
                width: Math.round(parent.width * 0.6)
                height: width
                source: root.hasAppIcon ? Quickshell.iconPath(root.notif?.appIcon ?? "", true) : ""
                sourceSize.width: 34
                sourceSize.height: 34
                asynchronous: true
            }
        }

        Text {
            id: appName

            anchors.top: parent.top
            anchors.left: image.right
            anchors.leftMargin: 10

            text: appNameMetrics.elidedText
            maximumLineCount: 1
            color: Colors.textMuted
            font.pixelSize: 10
            font.family: Fonts.family

            opacity: root.expanded ? 1 : 0

            Behavior on opacity {
                Anim {
                    duration: 150
                    easing.bezierCurve: [0.34, 0.8, 0.34, 1, 1, 1]
                }
            }
        }

        TextMetrics {
            id: appNameMetrics

            text: root.notif?.appName ?? ""
            font: appName.font
            elide: Text.ElideRight
            elideWidth: expandBtn.x - time.width - timeSep.width - summary.x - 20
        }

        Text {
            id: summary

            anchors.top: parent.top
            anchors.left: image.right
            anchors.leftMargin: 10

            text: summaryMetrics.elidedText
            maximumLineCount: 1
            height: implicitHeight
            color: Colors.textBright
            font.pixelSize: 12
            font.family: Fonts.family
            font.weight: Font.Medium

            states: State {
                name: "expanded"
                when: root.expanded

                PropertyChanges {
                    summary.maximumLineCount: undefined
                    summary.anchors.topMargin: 4
                    bodyPreview.anchors.topMargin: 4
                    body.anchors.topMargin: 4
                }

                AnchorChanges {
                    target: summary
                    anchors.top: appName.bottom
                }
            }

            transitions: Transition {
                PropertyAction {
                    target: summary
                    property: "maximumLineCount"
                }
                Anim {
                    property: "topMargin"
                }
                AnchorAnimation {
                    duration: 300
                    easing.type: Easing.BezierSpline
                    easing.bezierCurve: [0.2, 0, 0, 1, 1, 1]
                }
            }

            Behavior on height {
                Anim {}
            }
        }

        TextMetrics {
            id: summaryMetrics

            text: root.notif?.summary ?? ""
            font: summary.font
            elide: Text.ElideRight
            elideWidth: expandBtn.x - time.width - timeSep.width - summary.x - 20
        }

        Text {
            id: timeSep

            anchors.top: parent.top
            anchors.topMargin: 1
            anchors.left: summary.right
            anchors.leftMargin: 6

            text: "•"
            color: Colors.textMuted
            font.pixelSize: 10
            font.family: Fonts.family

            states: State {
                name: "expanded"
                when: root.expanded

                AnchorChanges {
                    target: timeSep
                    anchors.left: appName.right
                }
            }

            transitions: Transition {
                AnchorAnimation {
                    duration: 300
                    easing.type: Easing.BezierSpline
                    easing.bezierCurve: [0.2, 0, 0, 1, 1, 1]
                }
            }
        }

        Text {
            id: time

            anchors.top: parent.top
            anchors.topMargin: 1
            anchors.left: timeSep.right
            anchors.leftMargin: 6

            text: root.notif?.timeStr ?? ""
            color: Colors.textMuted
            font.pixelSize: 10
            font.family: Fonts.family
        }

        Item {
            id: expandBtn

            anchors.right: parent.right
            anchors.top: parent.top
            anchors.topMargin: -2

            implicitWidth: 20
            implicitHeight: 20

            Rectangle {
                anchors.fill: parent
                radius: width / 2
                color: expandMouse.containsMouse ? Colors.subtle : "transparent"

                Behavior on color {
                    ColorAnimation {
                        duration: 150
                    }
                }
            }

            MaterialIcon {
                anchors.centerIn: parent
                text: "expand_more"
                size: 16
                rotation: root.expanded ? 180 : 0

                Behavior on rotation {
                    Anim {}
                }
            }

            MouseArea {
                id: expandMouse

                anchors.fill: parent
                hoverEnabled: true
                cursorShape: Qt.PointingHandCursor
                onClicked: root.expanded = !root.expanded
            }
        }

        Text {
            id: bodyPreview

            anchors.left: summary.left
            anchors.right: expandBtn.left
            anchors.top: summary.bottom
            anchors.rightMargin: 6

            text: bodyPreviewMetrics.elidedText
            color: Colors.textMuted
            font.pixelSize: 10
            font.family: Fonts.family

            opacity: root.expanded ? 0 : 1

            Behavior on opacity {
                Anim {
                    duration: 150
                    easing.bezierCurve: [0.34, 0.8, 0.34, 1, 1, 1]
                }
            }
        }

        TextMetrics {
            id: bodyPreviewMetrics

            text: root.notif?.body ?? ""
            font: bodyPreview.font
            elide: Text.ElideRight
            elideWidth: bodyPreview.width
        }

        Text {
            id: body

            anchors.left: summary.left
            anchors.right: expandBtn.left
            anchors.top: summary.bottom
            anchors.rightMargin: 6

            text: root.notif?.body ?? ""
            color: Colors.textMuted
            font.pixelSize: 10
            font.family: Fonts.family
            wrapMode: Text.WrapAtWordBoundaryOrAnywhere
            height: text ? implicitHeight : 0

            opacity: root.expanded ? 1 : 0

            Behavior on opacity {
                Anim {
                    duration: 150
                    easing.bezierCurve: [0.34, 0.8, 0.34, 1, 1, 1]
                }
            }
        }

        Row {
            id: actions

            anchors.left: body.left
            anchors.right: body.right
            anchors.top: body.bottom
            anchors.topMargin: 8

            spacing: 4
            opacity: root.expanded ? 1 : 0

            Behavior on opacity {
                Anim {
                    duration: 150
                    easing.bezierCurve: [0.34, 0.8, 0.34, 1, 1, 1]
                }
            }

            Btn {
                width: 26
                icon: "close"
                onTriggered: root.notif?.dismiss()
            }

            Repeater {
                model: root.notif?.actions ?? []

                Btn {
                    required property var modelData

                    width: (actions.width - 26 * 2 - 4 * (root.actionCount + 1)) / root.actionCount
                    label: modelData.text
                    onTriggered: {
                        modelData.invoke();
                        root.notif?.dismiss();
                    }
                }
            }

            Btn {
                width: 26
                icon: copyTimer.running ? "inventory" : "content_copy"
                onTriggered: {
                    Quickshell.clipboardText = root.notif?.body ?? "";
                    copyTimer.restart();
                }

                Timer {
                    id: copyTimer

                    interval: 3000
                }
            }
        }
    }

    component Btn: Rectangle {
        id: btn

        property string icon
        property string label

        signal triggered

        height: 26
        radius: height / 2
        color: btnMouse.containsMouse ? Colors.surfaceActive : Colors.subtle

        MaterialIcon {
            anchors.centerIn: parent
            text: btn.icon
            size: 14
            visible: btn.icon.length > 0
        }

        Text {
            anchors.centerIn: parent
            width: parent.width - 16
            text: btn.label
            color: Colors.textDimmed
            font.pixelSize: 10
            font.family: Fonts.family
            font.weight: Font.Medium
            elide: Text.ElideRight
            horizontalAlignment: Text.AlignHCenter
            visible: btn.label.length > 0
        }

        MouseArea {
            id: btnMouse

            anchors.fill: parent
            hoverEnabled: true
            cursorShape: Qt.PointingHandCursor
            onClicked: btn.triggered()
        }

        Behavior on color {
            ColorAnimation {
                duration: 150
            }
        }
    }
}
