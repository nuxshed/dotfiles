pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import Quickshell
import "../../components"
import "../../config"
import "../../services"

Item {
    id: root

    required property string modelData
    property bool expanded: false

    readonly property var items: Notifications.items(modelData)
    readonly property var head: items[0] ?? null
    readonly property int count: items.length
    readonly property bool stacked: count > 1 && !expanded
    readonly property int layers: stacked ? Math.min(2, count - 1) : 0

    implicitHeight: header.height + 6 + list.height + layers * 5

    onCountChanged: {
        if (count <= 1)
            expanded = false;
    }

    HoverHandler {
        id: hover
    }

    RowLayout {
        id: header

        width: parent.width
        height: 20
        spacing: 6

        Image {
            Layout.preferredWidth: 14
            Layout.preferredHeight: 14
            source: {
                const icon = root.head?.icon ?? "";
                if (icon.length === 0)
                    return "";
                if (icon.startsWith("/"))
                    return "file://" + icon;
                return icon.includes("://") ? icon : Quickshell.iconPath(icon, true);
            }
            sourceSize: Qt.size(28, 28)
            asynchronous: true
            visible: status === Image.Ready
        }

        Text {
            text: root.head?.appName ?? ""
            color: Colors.textDimmed
            font.pixelSize: 10
            font.family: Fonts.family
            font.weight: Font.Medium
            elide: Text.ElideRight
            Layout.maximumWidth: 200
        }

        Rectangle {
            visible: root.count > 1
            implicitWidth: countText.implicitWidth + 10
            implicitHeight: 14
            radius: 7
            color: Colors.surface

            Text {
                id: countText

                anchors.centerIn: parent
                text: root.count
                color: Colors.textMuted
                font.pixelSize: 8
                font.family: Fonts.family
                font.weight: Font.Medium
            }
        }

        Item {
            Layout.fillWidth: true
        }

        HeaderButton {
            visible: root.count > 1
            icon: "expand_more"
            label: root.expanded ? "Less" : "Show all"
            flip: root.expanded
            onClicked: root.expanded = !root.expanded
        }

        HeaderButton {
            icon: "close"
            opacity: hover.hovered ? 1 : 0
            onClicked: Notifications.dismissGroup(root.modelData)

            Behavior on opacity {
                NumberAnimation {
                    duration: 120
                }
            }
        }
    }

    Repeater {
        model: root.layers

        Rectangle {
            required property int index

            x: 8 * (index + 1)
            y: list.y + list.height - 10 + 5 * (index + 1)
            z: -1 - index
            width: root.width - 16 * (index + 1)
            height: 16
            radius: 10
            color: Colors.surface
            opacity: 0.7 - 0.25 * index
        }
    }

    ListView {
        id: list

        y: header.height + 6
        width: parent.width
        height: contentHeight
        spacing: 4
        interactive: false

        Behavior on height {
            Anim {
                duration: 260
            }
        }

        model: ScriptModel {
            values: root.expanded ? root.items : root.items.slice(0, 1)
        }

        delegate: NotifCard {
            id: card

            required property var modelData

            width: list.width
            notif: modelData
            onTapped: {
                if (root.stacked)
                    root.expanded = true;
                else
                    card.dismiss();
            }
        }

        add: Transition {
            NumberAnimation {
                property: "opacity"
                from: 0
                to: 1
                duration: 200
            }
            NumberAnimation {
                property: "scale"
                from: 0.96
                to: 1
                duration: 240
                easing.type: Easing.OutCubic
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
                duration: 240
                easing.type: Easing.OutCubic
            }
            NumberAnimation {
                property: "opacity"
                to: 1
                duration: 150
            }
            NumberAnimation {
                property: "scale"
                to: 1
                duration: 150
            }
        }
    }

    component HeaderButton: Rectangle {
        id: hb

        property string icon
        property string label
        property bool flip: false

        signal clicked

        implicitWidth: row.implicitWidth + (label.length > 0 ? 14 : 4)
        implicitHeight: 20
        radius: 10
        color: hbMouse.containsMouse ? Colors.surfaceActive : Colors.surface

        Behavior on color {
            ColorAnimation {
                duration: 120
            }
        }

        Row {
            id: row

            anchors.centerIn: parent
            spacing: 2

            Text {
                anchors.verticalCenter: parent.verticalCenter
                text: hb.label
                visible: hb.label.length > 0
                color: Colors.textDimmed
                font.pixelSize: 9
                font.family: Fonts.family
                font.weight: Font.Medium
            }

            MaterialIcon {
                anchors.verticalCenter: parent.verticalCenter
                text: hb.icon
                size: 13
                rotation: hb.flip ? 180 : 0

                Behavior on rotation {
                    Anim {}
                }
            }
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
